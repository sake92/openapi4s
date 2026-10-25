package ba.sake.openapi4s
package http4s

import com.typesafe.scalalogging.LazyLogging

import java.nio.file.Paths
import scala.collection.mutable
import scala.meta._
import scala.meta.dialects.Scala34
import org.apache.commons.text.CaseUtils
import ba.sake.regenesca._
import ba.sake.openapi4s.exceptions.UnsupportedSchemaException

class Http4sGenerator(
    config: OpenApiWriter.Config,
    openApiDefinition: OpenApiDefinition,
    frameworkModelImports: List[Import]
) extends OpenApiGenerator
    with LazyLogging {

  override def generate(): Seq[GeneratedFileSource] = {
    val groupedByTag = openApiDefinition.pathDefinitions.defs.groupBy(_.getTag)
    groupedByTag.flatMap { case (tag, pathDefinitions) =>
      generateRouteSources(tag, pathDefinitions)
    }
  }.toList

  private def generateRouteSources(
      tag: String,
      pathDefinitions: List[PathDefinition]
  ): List[GeneratedFileSource] = {
    val controllerName = CaseUtils.toCamelCase(tag, true, '_') + "Routes"
    val controllerTypeName = Type.Name(controllerName)
    val matcherNames = mutable.LinkedHashMap.empty[QueryParam, Term.Name]
    val routeDefinitions = pathDefinitions.map { pathDef =>
      val pathSegmentPatterns = pathDef.pathSegments.map {
        case PathSegment.Literal(value) => Lit.String(value)
        case PathSegment.Param(name, schema) =>
          try {
            SchemaUtils.resolveType(
              schema,
              None,
              None,
              allowNullable = false,
              s"${pathDef.method} '${pathDef.path}' path param",
              fallbackAnyType = t"String"
            )
            Pat.Var(ScalaIdents.termName(name))
          } catch {
            case e: UnsupportedSchemaException =>
              logger.warn(e.toString)
              Pat.Var(ScalaIdents.termName(name))
          }
      }

      val pathExtract =
        if (pathSegmentPatterns.isEmpty) p"Root"
        else
          pathSegmentPatterns.tail.foldLeft(p"Root / ${pathSegmentPatterns.head}") { (a, b) =>
            p"${a} / ${b}"
          }

      // IDK how to fold this *hit
      val pathExtractClause = Pat.ExtractInfix(
        lhs = Term.Name(pathDef.method.toUpperCase),
        op = Term.Name("->"),
        argClause = Pat.ArgClause(List(pathExtract))
      )

      val pathBindings = pathDef.pathSegments.collect { case PathSegment.Param(name, _) =>
        ScalaIdents.termName(name).value
      }
      val takenBindings = mutable.Set("req", "reqBody", "resp") ++ pathBindings
      val (queryMatchers, queryPatterns) = pathDef.queryParams.zipWithIndex.map { case (qp, paramIndex) =>
        val matcherName = matcherNames.getOrElseUpdate(
          qp, {
            val words = qp.name.replaceAll("[^a-zA-Z0-9]", "_")
            val camelizedName = CaseUtils.toCamelCase(words, true, '_') + "QueryParamMatcher"
            val baseName = if (ScalaIdents.isValid(camelizedName)) camelizedName else s"Query$camelizedName"
            val usedNames = matcherNames.values.map(_.value).toSet
            val candidates = Iterator.single(baseName) ++ Iterator.from(2).map(i => s"$baseName$i")
            Term.Name(candidates.find(name => !usedNames(name)).get)
          }
        )
        generateQueryParam(qp, matcherName, paramIndex, takenBindings)
      }.unzip
      val routePattern = queryPatterns.headOption match {
        case Some(first) =>
          val combined = queryPatterns.tail.foldLeft(first) { (left, right) => p"$left +& $right" }
          p"$pathExtractClause :? $combined"
        case None => pathExtractClause
      }

      val reqBodyStmts = pathDef.reqBody.flatMap { body =>
        try {
          val tpe =
            SchemaUtils.resolveType(
              body.schema,
              None,
              None,
              allowNullable = true,
              s"${pathDef.method} '${pathDef.path}' req body",
              fallbackAnyType = t"JValue"
            )
          // val finalTpe = if (body.required) tpe else t"Option[$tpe]"
          Some(enumerator"reqBody <- req.as[${tpe}]")
        } catch {
          case e: UnsupportedSchemaException =>
            logger.warn(e.toString)
            None
        }
      }.toList
      val resBodyExpr = pathDef.resBody
        .flatMap { body =>
          try {
            val tpe = SchemaUtils.resolveType(
              body.schema,
              None,
              None,
              allowNullable = true,
              s"${pathDef.method} '${pathDef.path}' res body",
              fallbackAnyType = t"Json"
            )
            val todoBody = Lit.String(s"TODO: return ${tpe}")
            Some(enumerator" resp <- NotImplemented(${todoBody}) ")
          } catch {
            case e: UnsupportedSchemaException =>
              logger.warn(e.toString)
              None
          }
        }
        .getOrElse(enumerator" resp <- NotImplemented() ")
      val routeStmts = List(q"""
          for {
            ..${reqBodyStmts ++ List(resBodyExpr)}
          } yield resp
        """)
      val pathDefCase =
        p"""case req @ ${routePattern} =>
                { ..${routeStmts} }
        """
      (queryMatchers.flatten, pathDefCase)
    }
    val (queryMatchers, casesnel) = routeDefinitions.unzip
    val matcherDefinitions = queryMatchers.flatten.distinctBy(_.syntax)
    val pkg = generatePkgSelect(s"${config.basePackage}.routes")
    val imports = List[Import](
      q"import java.time.*",
      q"import java.util.UUID",
      q"import cats.effect.IO",
      q"import org.http4s.{HttpRoutes, QueryParamDecoder}",
      q"import org.http4s.dsl.io._"
    ) ++ frameworkModelImports ++ List(GenerationImports.modelWildcardImport(config.basePackage))
    val generatedSource = source"""
        package ${pkg} {
            ..${imports}
            class ${controllerTypeName} {
                ..${matcherDefinitions};
                def routes = HttpRoutes.of[IO] {
                  ..case ${casesnel}
                }
            }
        }
        """
    // Parse the final text so TODO comments survive scala.meta's structural rendering.
    val decoderStubs = matcherDefinitions.collect { case stub: Defn.GivenAlias => stub.syntax }
    val sourceWithTodos = decoderStubs.foldLeft(generatedSource.syntax) { (code, stub) =>
      code.replace(stub, s"// TODO: implement query parameter decoding\n  $stub")
    }
    List(GeneratedFileSource(Paths.get(s"routes/${controllerName}.scala"), sourceWithTodos.parse[Source].get))
  }

  private def generateQueryParam(
      qp: QueryParam,
      matcherName: Term.Name,
      paramIndex: Int,
      takenBindings: mutable.Set[String]
  ): (List[Stat], Pat) = {
    val schema = resolveQuerySchema(qp.schema)
    val (elementSchema, isArray) = schema match {
      case SchemaDefinition.Arr(inner, _, _, _) => (resolveQuerySchema(inner), true)
      case other                                => (other, false)
    }
    val tpe = queryParamType(elementSchema)
    val builtInTypes = Set("String", "Boolean", "Int", "Long", "Float", "Double")
    val decoderStubs =
      if (builtInTypes(tpe.syntax)) Nil
      else List(q"given QueryParamDecoder[$tpe] = ???")
    val matcherType =
      if (isArray && !qp.required) t"OptionalMultiQueryParamDecoderMatcher[$tpe]"
      else if (!qp.required) t"OptionalQueryParamDecoderMatcher[$tpe]"
      else t"QueryParamDecoderMatcher[$tpe]"
    val wireName = Lit.String(qp.name)
    val matcherInit = init"$matcherType($wireName)"
    val matcher =
      if (isArray && qp.required) q"""
      private object $matcherName {
        private val matcher = new $matcherInit {}
        def unapply(params: Map[String, collection.Seq[String]]): Option[collection.Seq[$tpe]] =
          matcher.unapplySeq(params)
      }
    """
      else q"private object $matcherName extends $matcherInit"
    // Backticked identifiers in patterns refer to stable values instead of binding new variables.
    val baseBindingName =
      if (ScalaIdents.isValid(qp.name) && qp.name.head.isLower) ScalaIdents.termName(qp.name).value
      else s"queryParam$paramIndex"
    var bindingName = baseBindingName
    while (takenBindings(bindingName)) bindingName += "_"
    takenBindings += bindingName
    val binding = Pat.Var(ScalaIdents.termName(bindingName))
    val pattern =
      if (isArray && !qp.required) p"$matcherName(cats.data.Validated.Valid($binding))"
      else p"$matcherName($binding)"
    (decoderStubs :+ matcher, pattern)
  }

  private def resolveQuerySchema(schema: SchemaDefinition, seen: Set[String] = Set.empty): SchemaDefinition =
    schema match {
      case SchemaDefinition.Opt(inner)                                 => resolveQuerySchema(inner, seen)
      case named @ SchemaDefinition.Named(_, _: SchemaDefinition.Enum) => named
      case SchemaDefinition.Named(_, inner)                            => resolveQuerySchema(inner, seen)
      case SchemaDefinition.Ref(name) if !seen(name) =>
        val named = openApiDefinition.namedSchemaDefinitions.defs.find(_.name == name).getOrElse {
          throw new UnsupportedSchemaException(s"Unknown query parameter schema '$name'")
        }
        resolveQuerySchema(named, seen + name)
      case _: SchemaDefinition.Ref =>
        throw new UnsupportedSchemaException("Recursive query parameter schemas are not supported")
      case other => other
    }

  private def queryParamType(schema: SchemaDefinition): Type = schema match {
    case _: SchemaDefinition.Enum         => t"String"
    case SchemaDefinition.Const(value, _) => literalType(value)
    case other =>
      SchemaUtils.resolveType(
        other,
        None,
        None,
        allowNullable = false,
        "http4s query parameter",
        fallbackAnyType = t"String"
      )
  }

  private def literalType(value: EnumLiteral): Type = value match {
    case _: EnumLiteral.StrValue  => t"String"
    case _: EnumLiteral.IntValue  => t"Int"
    case _: EnumLiteral.LongValue => t"Long"
    case _: EnumLiteral.NumValue  => t"Double"
    case _: EnumLiteral.BoolValue => t"Boolean"
  }

  private def generatePkgSelect(pkg: String) = {
    val packageComponents = pkg.split("\\.").toList
    val firstSelect = q"${Term.Name(packageComponents(0))}.${Term.Name(packageComponents(1))}"
    packageComponents.tail.tail.foldLeft(firstSelect) { (a, b) =>
      q"${a}.${Term.Name(b)}"
    }
  }

}
