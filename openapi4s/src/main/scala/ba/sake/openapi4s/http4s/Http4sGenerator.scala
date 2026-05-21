package ba.sake.openapi4s
package http4s

import java.nio.file.Paths
import scala.meta._
import scala.meta.dialects.Scala34
import org.apache.commons.text.CaseUtils
import ba.sake.regenesca._
import ba.sake.openapi4s.exceptions.UnsupportedSchemaException
import ba.sake.openapi4s.circe.CirceModelGenerator

class Http4sGenerator(
    config: OpenApiWriter.Config,
    openApiDefinition: OpenApiDefinition,
    frameworkModelImports: List[Import]
) extends OpenApiGenerator {

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
    val casesnel = pathDefinitions.map { pathDef =>
      val pathSegmentPatterns = pathDef.pathSegments.map {
        case PathSegment.Literal(value) => Lit.String(value)
        case PathSegment.Param(name, schema) =>
          val tpe =
            SchemaUtils.resolveType(
              schema,
              None,
              None,
              allowNullable = false,
              s"${pathDef.method} '${pathDef.path}' path param",
              fallbackAnyType = t"String"
            )
          if (tpe.structure == t"String".structure) Pat.Var(Term.Name(name))
          else {
            val paramName = Pat.Var(Term.Name(name))
            // p"param[${tpe}]($paramName)"
            p"$paramName"
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

      // TODO handle QPs https://http4s.org/v0.21/dsl/
      /*val queryParamStmts = Option
        .when(pathDef.queryParams.nonEmpty) {
          val (qpParams, adhocEnums) = pathDef.queryParams.flatMap { qp =>
            val adhocEnumOpt = Option.when(qp.schema.isInstanceOf[SchemaDefinition.Enum]) {
              val adhocEnumName = generateEnumName("QP", qp.name)
              val adhocEnumType = Type.Name(adhocEnumName)
              val enumCaseDefs = Defn.RepeatedEnumCase(
                List.empty,
                qp.schema.asInstanceOf[SchemaDefinition.Enum].values.map { enumDefCaseValue =>
                  Term.Name(enumDefCaseValue)
                }
              )
              q"""enum ${adhocEnumType} derives QueryStringRW {
                    ${enumCaseDefs}
                }"""
            }
            val qpName = Name(qp.name)
            try {
              val tpe = resolveType(
                qp.schema,
                Some(qp.name),
                Some("QP"),
                allowNullable = true,
                s"${pathDef.method} '${pathDef.path}' query param",
                fallbackAnyType = t"String"
              )
              val finalTpe = if (qp.required) tpe else t"Option[$tpe]"
              Some((param"${qpName}: ${finalTpe}", adhocEnumOpt))
            } catch {
              case e: UnsupportedSchemaException =>
                println(e.toString)
                None
            }
          }.unzip
          // validation
          // TODO figure out how to validate Option-al nicely
          val validatedQPs = pathDef.queryParams.filter(_.required).map(qp => (qp.name, qp.schema))
          val validatorStmts = SchemaUtils.generateValidsonStms(t"QP", validatedQPs)
          adhocEnums.flatten ++
            List(q"case class QP(..${qpParams}) derives QueryStringRW") ++
            Option.when(validatorStmts.nonEmpty)(q""" object QP { ..${validatorStmts} } """).toList ++
            List(q"val qp = Request.current.queryParamsValidated[QP]")
        }
        .toList
        .flatten
       */
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
            println(e.toString)
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
              println(e.toString)
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
        p"""case req @ ${pathExtractClause} =>
                { ..${routeStmts} }
        """
      pathDefCase
    }
    val pkg = generatePkgSelect(s"${config.basePackage}.routes")
    val imports = List[Import](
      q"import java.time.*",
      q"import java.util.UUID",
      q"import cats.effect.IO",
      q"import org.http4s.HttpRoutes",
      q"import org.http4s.dsl.io._"
    ) ++ frameworkModelImports ++ List(GenerationImports.modelWildcardImport(config.basePackage))
    List(
      GeneratedFileSource(
        Paths.get(s"routes/${controllerName}.scala"),
        source"""
        package ${pkg} {
            ..${imports}
            class ${controllerTypeName} {
                def routes = HttpRoutes.of[IO] {
                  ..case ${casesnel}
                }
            }
        }
        """
      )
    )
  }

  private def generatePkgSelect(pkg: String) = {
    val packageComponents = pkg.split("\\.").toList
    val firstSelect = q"${Term.Name(packageComponents(0))}.${Term.Name(packageComponents(1))}"
    packageComponents.tail.tail.foldLeft(firstSelect) { (a, b) =>
      q"${a}.${Term.Name(b)}"
    }
  }

}
