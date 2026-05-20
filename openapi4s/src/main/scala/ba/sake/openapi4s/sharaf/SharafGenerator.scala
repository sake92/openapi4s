package ba.sake.openapi4s
package sharaf

import java.nio.file.Paths
import scala.meta._
import scala.meta.dialects.Scala34
import org.apache.commons.text.CaseUtils
import ba.sake.regenesca._
import ba.sake.openapi4s.exceptions.UnsupportedSchemaException
import ba.sake.openapi4s.tupson.TupsonModelGenerator

class SharafGenerator(
    config: OpenApiGenerator.Config,
    openApiDefinition: OpenApiDefinition,
    modelFileImports: List[Import] = ModelImportContracts.tupson.modelFileImports,
    frameworkModelImports: List[Import] = ModelImportContracts.tupson.frameworkImports("sharaf")
) extends OpenApiGenerator {

  private val merger = SourceMerger(mergeDefBodies = true)
  private val regenescaGenerator = RegenescaGenerator(merger)

  override def generate(): Unit = {
    println(s"Started generating Sharaf server for '${config.url}' OpenApi into '${config.baseFolder}' ...")
    val packagePath = config.basePackage.replaceAll("\\.", "/")
    val adaptedGenSourceFiles = generateSources.map { gsf =>
      gsf.copy(file = config.baseFolder.resolve(packagePath).resolve(gsf.file.toString))
    }
    regenescaGenerator.generate(adaptedGenSourceFiles)
    println(s"Finished generating Sharaf server for '${config.url}' OpenApi.")
  }

  private[openapi4s] def generateSources: Seq[GeneratedFileSource] = {
    val modelsPkg = generatePkgSelect(s"${config.basePackage}.models")
    val modelGenerator = new TupsonModelGenerator(openApiDefinition)
    val modelFileSources = openApiDefinition.namedSchemaDefinitions.defs.flatMap { namedSchemaDef =>
      val namedSchemaName = namedSchemaDef.name.capitalize
      val modelSources = modelGenerator.generateModelSources(namedSchemaDef, None)
      val allStmts = modelFileImports ++ modelSources
      Option.when(modelSources.nonEmpty) {
        GeneratedFileSource(
          Paths.get(s"models/${namedSchemaName}.scala"),
          source"""
            // generated with OpenApi4s
            package ${modelsPkg} { ..${allStmts} }
          """
        )
      }
    }
    val controllerFileSources = generateControllersSources
    modelFileSources ++ controllerFileSources
  }

  private def generateControllersSources: List[GeneratedFileSource] = {
    val groupedByTag = openApiDefinition.pathDefinitions.defs.groupBy(_.getTag)
    groupedByTag.flatMap { case (tag, pathDefinitions) =>
      generateControllerSources(tag, pathDefinitions)
    }
  }.toList

  private def generateControllerSources(
      tag: String,
      pathDefinitions: List[PathDefinition]
  ): List[GeneratedFileSource] = {
    val controllerName = CaseUtils.toCamelCase(tag, true, '_') + "Controller"
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
            p"param[${tpe}]($paramName)"
          }
      }
      val pathSegmentPatternsClause = Pat.ArgClause(pathSegmentPatterns)
      val methodExtractor = Term.Name(pathDef.method.toUpperCase)
      val queryParamStmts = Option
        .when(pathDef.queryParams.nonEmpty) {
          val (qpParams, adhocEnums) = pathDef.queryParams.flatMap { qp =>
            val adhocEnumOpt = Option.when(qp.schema.isInstanceOf[SchemaDefinition.Enum]) {
              val adhocEnumName = SchemaUtils.generateEnumName("QP", qp.name)
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
              val tpe = SchemaUtils.resolveType(
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
          Some(q"val reqBody = Request.current.bodyJsonValidated[${tpe}]")
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
              fallbackAnyType = t"JValue"
            )
            val todoBody = Lit.String(s"TODO: return ${tpe}")
            Some(q"""Response.withStatus(StatusCode.NotImplemented).withBody(${todoBody})""")
          } catch {
            case e: UnsupportedSchemaException =>
              println(e.toString)
              None
          }
        }
        .getOrElse(q"Response.withStatus(StatusCode.NotImplemented)")
      val routeStmts = queryParamStmts ++ reqBodyStmts ++ List(resBodyExpr)
      val pathDefCase =
        p"""case ${methodExtractor} -> Path(..${pathSegmentPatternsClause}) =>
                { ..${routeStmts} }
        """
      pathDefCase
    }
    val pkg = generatePkgSelect(s"${config.basePackage}.controllers")
    val imports = List[Import](
      q"import java.time.*",
      q"import java.util.UUID",
      q"import sttp.model.StatusCode",
      q"import ba.sake.querson.QueryStringRW",
      q"import ba.sake.validson.Validator",
      q"import ba.sake.sharaf.*, routing.*"
    ) ++ frameworkModelImports ++ List(GenerationImports.modelWildcardImport(config.basePackage))
    List(
      GeneratedFileSource(
        Paths.get(s"controllers/${controllerName}.scala"),
        source"""
        // generated with OpenApi4s
        package ${pkg} {
            ..${imports}

            class ${controllerTypeName} {
                def routes = Routes{ ..case ${casesnel} }
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
