package ba.sake.openapi4s
package sttp

import java.nio.file.Paths
import scala.collection.mutable
import scala.meta._
import scala.meta.dialects.Scala34
import org.apache.commons.text.CaseUtils
import ba.sake.regenesca._
import ba.sake.openapi4s.exceptions.UnsupportedSchemaException

/** Generates sttp client4 HTTP clients. One `XClient` class per OpenAPI tag, methods per operation.
  *
  * Generated code targets sttp client4 (verified against 4.0.x):
  *   - `import sttp.client4.*`, `import sttp.client4.circe.*`
  *   - `asJson[B]: ResponseAs[Either[ResponseException[String], B]]`
  *   - uri interpolator drops `None` query params
  *   - `ResponseAs.deserializeRightCatchingExceptions` for the tupson helper
  */
class SttpClientGenerator(
    config: OpenApiWriter.Config,
    openApiDefinition: OpenApiDefinition,
    modelContract: ModelContract
) extends OpenApiGenerator {

  override def generate(): Seq[GeneratedFileSource] = {
    val groupedByTag = openApiDefinition.pathDefinitions.defs.groupBy(_.getTag)
    groupedByTag.flatMap { case (tag, pathDefinitions) =>
      generateClientSources(tag, pathDefinitions)
    }.toList
  }

  private def generateClientSources(
      tag: String,
      pathDefinitions: List[PathDefinition]
  ): List[GeneratedFileSource] = {
    val clientName = CaseUtils.toCamelCase(tag, true, '_') + "Client"
    val clientTypeName = Type.Name(clientName)
    val takenMethodNames = mutable.Set.empty[String]
    val methodDefs = pathDefinitions.map { pathDef =>
      generateMethodDef(pathDef, takenMethodNames)
    }
    val serverVals = openApiDefinition.servers.zipWithIndex.map { case (url, idx) =>
      q"val ${Pat.Var(Term.Name(s"server${idx + 1}"))}: String = ${Lit.String(url)}"
    }
    val companionObj = Option
      .when(serverVals.nonEmpty)(q"object ${Term.Name(clientName)} { ..${serverVals} }")
      .toList
    val pkg = generatePkgSelect(s"${config.basePackage}.clients")
    List(
      GeneratedFileSource(
        Paths.get(s"clients/${clientName}.scala"),
        source"""
        // generated with OpenApi4s
        package ${pkg} {
            ..${generateImports()}
            ..${companionObj}
            class ${clientTypeName}(baseUrl: String) {
                ..${methodDefs}
            }
        }
        """
      )
    )
  }

  private def generateImports(): List[Import] = {
    val jsonImports =
      if (config.models == "tupson") List.empty
      else List(q"import sttp.client4.circe.*")
    List(q"import sttp.client4.*") ++ jsonImports ++ List(
      GenerationImports.modelWildcardImport(config.basePackage)
    )
  }

  private def generateMethodDef(
      pathDef: PathDefinition,
      takenMethodNames: mutable.Set[String]
  ): Defn.Def = {
    val methodName = uniqueName(methodBaseName(pathDef), takenMethodNames)

    val params = mutable.ListBuffer.empty[Term.Param]

    // path params
    pathDef.pathSegments.foreach {
      case PathSegment.Param(name, schema) =>
        resolveParamType(
          schema,
          allowNullable = false,
          s"${pathDef.method} '${pathDef.path}' path param"
        ).foreach { tpe =>
          params += param"${ScalaIdents.termName(name)}: ${tpe}"
        }
      case _: PathSegment.Literal =>
    }
    // query params
    pathDef.queryParams.foreach { qp =>
      resolveParamType(
        qp.schema,
        allowNullable = true,
        s"${pathDef.method} '${pathDef.path}' query param"
      ).foreach { tpe =>
        val finalTpe = if (qp.required) tpe else t"Option[$tpe]"
        params += param"${ScalaIdents.termName(qp.name)}: ${finalTpe}"
      }
    }
    // header params
    pathDef.headerParams.foreach { hp =>
      resolveParamType(
        hp.schema,
        allowNullable = true,
        s"${pathDef.method} '${pathDef.path}' header param"
      ).foreach { tpe =>
        val finalTpe = if (hp.required) tpe else t"Option[$tpe]"
        params += param"${ScalaIdents.termName(hp.name)}: ${finalTpe}"
      }
    }

    // build request: basicRequest.get(uri"...").header(...)...response(...)
    val uriTerm = buildUriTerm(pathDef)
    val httpMethod = Term.Name(pathDef.method.toLowerCase)
    var reqTerm: Term = Term.Apply(Term.Select(Term.Name("basicRequest"), httpMethod), List(uriTerm))
    pathDef.headerParams.filter(_.required).foreach { hp =>
      reqTerm = q"${reqTerm}.header(${Lit.String(hp.name)}, ${ScalaIdents.termName(hp.name)})"
    }
    reqTerm = q"${reqTerm}.response(${responseAsExpr(pathDef)})"

    // optional headers are applied via fold, keeping the request immutable
    val optionalHeaderFolds = pathDef.headerParams.filterNot(_.required)
    val foldExpr = optionalHeaderFolds.foldRight[Term](q"req") { (hp, acc) =>
      q"""${ScalaIdents.termName(hp.name)}.fold(${acc})(v => req.header(${Lit.String(hp.name)}, v))"""
    }

    val resTpe = responseType(pathDef)
    val bodyStmts =
      if (optionalHeaderFolds.isEmpty) List(reqTerm)
      else List(q"val req = ${reqTerm}", foldExpr)

    q"""def ${Term.Name(methodName)}(..${params.toList}): Request[Either[ResponseException[String], ${resTpe}]] = {
          ..${bodyStmts}
        }"""
  }

  private def responseAsExpr(pathDef: PathDefinition): Term = {
    if (config.models == "tupson") {
      pathDef.resBody match {
        case Some(body) => q"JsonSupport.asJson[${responseType(pathDef)}]"
        case None       => q"asString.map(_.map(_ => ${Lit.Unit()}))"
      }
    } else {
      pathDef.resBody match {
        case Some(_) => q"asJson[${responseType(pathDef)}]"
        case None    => q"asString.map(_.map(_ => ${Lit.Unit()}))"
      }
    }
  }

  private def responseType(pathDef: PathDefinition): Type = {
    pathDef.resBody match {
      case Some(body) =>
        try {
          SchemaUtils.resolveType(
            body.schema,
            None,
            None,
            allowNullable = true,
            s"${pathDef.method} '${pathDef.path}' res body",
            fallbackAnyType = t"String"
          )
        } catch {
          case e: UnsupportedSchemaException =>
            println(e.toString)
            t"String"
        }
      case None => t"Unit"
    }
  }

  /** Resolves a param schema to a Scala type. Unsupported schemas (e.g. inline enums) fall back to String, never
    * failing generation.
    */
  private def resolveParamType(
      schema: SchemaDefinition,
      allowNullable: Boolean,
      context: String
  ): Option[Type] = {
    try {
      Some(
        SchemaUtils.resolveType(
          schema,
          None,
          None,
          allowNullable = allowNullable,
          context,
          fallbackAnyType = t"String"
        )
      )
    } catch {
      case e: UnsupportedSchemaException =>
        println(e.toString)
        Some(t"String")
    }
  }

  /** Builds a `uri"..."` interpolator term, e.g. uri"$baseUrl/pet/$petId?status=$status&limit=$limit" */
  private def buildUriTerm(pathDef: PathDefinition): Term = {
    val parts = mutable.ListBuffer[String]("")
    val args = mutable.ListBuffer[Term](Term.Name("baseUrl"))
    val sb = new StringBuilder
    pathDef.pathSegments.foreach {
      case PathSegment.Literal(value) =>
        sb.append("/").append(value)
      case PathSegment.Param(name, _) =>
        sb.append("/")
        parts += sb.toString()
        sb.clear()
        args += Term.Name(name)
    }
    val pathStr = sb.toString()
    if (pathDef.queryParams.isEmpty) {
      parts += pathStr
    } else {
      pathDef.queryParams.zipWithIndex.foreach { case (qp, i) =>
        val prefix = if (i == 0) pathStr + "?" else "&"
        parts += prefix + qp.name + "="
        args += Term.Name(qp.name)
      }
      parts += ""
    }
    Term.Interpolate(Term.Name("uri"), parts.toList.map(Lit.String(_)), args.toList)
  }

  private def methodBaseName(pathDef: PathDefinition): String = {
    if (pathDef.operationId.nonEmpty) {
      // commons-text toCamelCase lowercases the whole string first,
      // so only camelize snake/kebab-case operationIds
      if (pathDef.operationId.exists(c => c == '_' || c == '-'))
        CaseUtils.toCamelCase(pathDef.operationId, false, '_', '-')
      else pathDef.operationId
    } else {
      val segments = pathDef.path
        .dropWhile(_ == '/')
        .split("/")
        .toList
        .map(seg => CaseUtils.toCamelCase(seg.replaceAll("[{}]", ""), true, '_'))
      (pathDef.method.toLowerCase :: segments).mkString
    }
  }

  private def uniqueName(base: String, taken: mutable.Set[String]): String = {
    if (!taken.contains(base)) {
      taken += base
      base
    } else {
      var i = 2
      while (taken.contains(base + i)) i += 1
      taken += base + i
      base + i
    }
  }

  private def generatePkgSelect(pkg: String) = {
    val packageComponents = pkg.split("\\.").toList
    val firstSelect = q"${Term.Name(packageComponents(0))}.${Term.Name(packageComponents(1))}"
    packageComponents.tail.tail.foldLeft(firstSelect) { (a, b) =>
      q"${a}.${Term.Name(b)}"
    }
  }
}
