package ba.sake.openapi4s
package sharaf

import java.nio.file.Paths
import scala.meta._
import scala.meta.dialects.Scala34
import org.apache.commons.text.CaseUtils
import ba.sake.regenesca._

class SharafGenerator extends OpenApiGenerator {

  private val merger = SourceMerger(mergeDefBody = true)
  private val regenescaGenerator = RegenescaGenerator(merger)

  override def generate(config: OpenApiGenerator.Config): Unit = {
    println(s"Started generating openapi '${config.url}' server into '${config.baseFolder}' ...")
    val openapiDefinition = OpenApiDefinition.parse(config.url)
    val packagePath = config.basePackage.replaceAll("\\.", "/")
    val genSourceFiles = generateSources(config, openapiDefinition)
    val adaptedGenSourceFiles = genSourceFiles.map { gsf =>
      gsf.copy(file = config.baseFolder.resolve(packagePath).resolve(gsf.file.toString))
    }
    regenescaGenerator.generate(adaptedGenSourceFiles)
    println(s"Finished generating openapi '${config.url}' server.")
  }

  private def generateSources(
      config: OpenApiGenerator.Config,
      openapiDefinition: OpenApiDefinition
  ): Seq[GeneratedFileSource] = {
    val modelSources = openapiDefinition.namedSchemaDefinitions.defs
      .flatMap(generateModelSources(config, _))
    val controllerSources = generateControllerSources(config, openapiDefinition.pathDefinitions.defs)
    modelSources ++ controllerSources
  }

  private def generateControllerSources(
      config: OpenApiGenerator.Config,
      pathDefs: Seq[PathDefinition]
  ): Seq[GeneratedFileSource] = {
    // TODO group by first tag, or "default" (main controller)
    val casesnel = pathDefs.map { pathDef =>
      val pathSegmentPatterns = pathDef.pathSegments.map {
        case PathSegment.Literal(value) => Lit.String(value)
        case PathSegment.Param(name, schema) =>
          val tpe = resolveType(schema, None, None, allowNullable = false)
          if (tpe.structure == t"String".structure) Pat.Var(Term.Name(name))
          else {
            val paramName = Pat.Var(Term.Name(name))
            p"param[${tpe}]($paramName)"
          }
      }
      val pathSegmentPatternsClause = Pat.ArgClause(pathSegmentPatterns.toList)
      val methodExtractor = Term.Name(pathDef.method.toUpperCase)
      val queryParamStmts = Option
        .when(pathDef.queryParams.nonEmpty) {
          val (qpParams, adhocEnums) = pathDef.queryParams
            .map { qp =>
              val adhocEnumOpt = Option.when(qp.schema.isInstanceOf[SchemaDefinition.Enum]) {
                val adhocEnumName = generateEnumName("QP", qp.name)
                val adhocEnumType = Type.Name(adhocEnumName)
                val enumCaseDefs = Defn.RepeatedEnumCase(
                  List.empty,
                  qp.schema.asInstanceOf[SchemaDefinition.Enum].values.toList.map { enumDefCaseValue =>
                    Term.Name(enumDefCaseValue)
                  }
                )
                q"""enum ${adhocEnumType} derives QueryStringRW {
                    ${enumCaseDefs}
                }"""
              }
              val qpName = Name(qp.name)
              val tpe = resolveType(qp.schema, Some(qp.name), Some("QP"))
              val finalTpe = if (qp.required) tpe else t"Option[$tpe]"
              (param"${qpName}: ${finalTpe}", adhocEnumOpt)
            }
            .toList
            .unzip
            // TODO validation
          adhocEnums.flatten ++ List(
            q"case class QP(..${qpParams}) derives QueryStringRW",
            q"val qp = Request.current.queryParamsValidated[QP]"
          )
        }
        .toList
        .flatten

      val reqBodyStmts = pathDef.reqBody.map { body =>
        val tpe = resolveType(body.schema, None, None)
        // val finalTpe = if (body.required) tpe else t"Option[$tpe]"
        q"val reqBody = Request.current.bodyJsonValidated[${tpe}]"
      }.toList
      val resBodyExpr = pathDef.resBody
        .map { body =>
          val tpe = resolveType(body.schema, None, None)
          val todoBody = Lit.String(s"TODO: return ${tpe}")
          q"""Response.withStatus(StatusCodes.NOT_IMPLEMENTED).withBody(${todoBody})"""
        }
        .getOrElse(q"Response.withStatus(StatusCodes.NOT_IMPLEMENTED)")
      val routeStmts = queryParamStmts ++ reqBodyStmts ++ List(resBodyExpr)
      val pathDefCase =
        p"""case ${methodExtractor}() -> Path(..${pathSegmentPatternsClause}) =>
                { ..${routeStmts} }
        """
      pathDefCase
    }.toList
    val pkg = generatePkgSelect(s"${config.basePackage}.controllers")
    val imports = List[Import](
      q"import io.undertow.util.StatusCodes",
      q"import ba.sake.querson.QueryStringRW",
      q"import ba.sake.sharaf.*, routing.*", {
        val importer = s"${config.basePackage}.models.*".parse[Importer].get
        q"import ..${List(importer)}"
      }
    )
    Seq(
      GeneratedFileSource(
        Paths.get(s"controllers/MainController.scala"),
        source"""
        package ${pkg} {
            ..${imports}

            class MainController {
                def routes = Routes{ ..case ${casesnel} }
            }
        }
        """
      )
    )
  }

  private def generateModelSources(
      config: OpenApiGenerator.Config,
      namedSchemaDef: SchemaDefinition.Named
  ): Seq[GeneratedFileSource] = {
    val typeName = Type.Name(namedSchemaDef.name.capitalize)
    val termName = Term.Name(namedSchemaDef.name.capitalize)
    val pkg = generatePkgSelect(s"${config.basePackage}.models")
    namedSchemaDef.schemaDef match {
      case obj: SchemaDefinition.Obj =>
        val params = obj.properties.map { property =>
          val propertyTpe = resolveType(
            property.tpe,
            Some(property.name),
            Some(namedSchemaDef.name)
          )
          param"${Term.Name(property.name)}: ${propertyTpe}"
        }
        val adHocEnums = obj.properties.flatMap { property =>
          val enumValuesOpt = property.tpe match {
            case SchemaDefinition.Enum(values)                       => Some(values)
            case SchemaDefinition.Opt(SchemaDefinition.Enum(values)) => Some(values)
            case SchemaDefinition.Arr(SchemaDefinition.Enum(values)) => Some(values)
            case _                                                   => None
          }
        enumValuesOpt.flatMap { values =>
          val adhocEnumName = generateEnumName(namedSchemaDef.name, property.name)
          val adhocEnumType = Type.Name(adhocEnumName)
          val enumCaseDefs = Defn.RepeatedEnumCase(
            List.empty,
            values.toList.map { enumDefCaseValue =>
              Term.Name(enumDefCaseValue)
            }
          )
          Some(
            GeneratedFileSource(
              Paths.get(s"models/${adhocEnumName}.scala"),
              source"""
                    package ${pkg} {
                        import ba.sake.tupson.JsonRW
                        enum ${adhocEnumType} derives JsonRW {
                            ${enumCaseDefs}
                        }
                    }
                    """
            )
          )
        }

        }
        Seq(
          // TODO validation
          GeneratedFileSource(
            Paths.get(s"models/${namedSchemaDef.name}.scala"),
            source"""
            package ${pkg} {
                import java.time.*
                import java.util.UUID
                import ba.sake.tupson.JsonRW
                import ba.sake.validson.Validator

                case class ${typeName}(
                ..${Term.ParamClause(params.toList)}
                ) derives JsonRW

                object ${termName} {
                    given Validator[${typeName}] = Validator
                        .derived[${typeName}]
                }
            }
            """
          )
        ) ++ adHocEnums
      case enumDef: SchemaDefinition.Enum =>
        val enumCaseDefs = Defn.RepeatedEnumCase(
          List.empty,
          enumDef.values.toList.map { enumDefCaseValue =>
            Term.Name(enumDefCaseValue)
          }
        )
        Seq(
          GeneratedFileSource(
            Paths.get(s"models/${namedSchemaDef.name}.scala"),
            source"""
            package ${pkg} {
                import ba.sake.tupson.JsonRW
                enum ${typeName} derives JsonRW {
                    ${enumCaseDefs}
                }
            }
            """
          )
        )
    }
  }

  private def resolveType(
      schemaDef: SchemaDefinition,
      propertyName: Option[String],
      parentTypeName: Option[String],
      // e.g. path enum cannot be null..
      allowNullable: Boolean = true
  ): Type = schemaDef match {
    case SchemaDefinition.Str()      => t"String"
    case SchemaDefinition.Int32()    => t"Int"
    case SchemaDefinition.Int64()    => t"Long"
    case SchemaDefinition.Double()   => t"Double"
    case SchemaDefinition.Bool()     => t"Boolean"
    case SchemaDefinition.UUID()     => t"UUID"
    case SchemaDefinition.Date()     => t"LocalDate"
    case SchemaDefinition.DateTime() => t"Instant"
    case SchemaDefinition.Opt(tpe) =>
      val coreTpe = resolveType(tpe, propertyName, parentTypeName)
      if (allowNullable) t"Option[${coreTpe}]"
      else coreTpe
    case SchemaDefinition.Arr(tpe) =>
      val coreTpe = resolveType(tpe, propertyName, parentTypeName)
      t"Seq[${coreTpe}]"
    case SchemaDefinition.Enum(_) =>
      (parentTypeName.zip(propertyName)) match {
        case Some((parentType, propName)) =>
          Type.Name(generateEnumName(parentType, propName))
        case _ =>
          throw new RuntimeException(
            s"Unsupported schema type : ${schemaDef.getClass}"
          )
      }
    case SchemaDefinition.Ref(name)      => Type.Name(name)
    case SchemaDefinition.Named(name, _) => Type.Name(name)
    case other =>
      throw new RuntimeException(s"Unsupported schema type : ${other.getClass}")
  }

  private def generateEnumName(parentType: String, propName: String): String = {
    val camelizedParentType = CaseUtils.toCamelCase(parentType, true, '_')
    val camelizedPropName = CaseUtils.toCamelCase(propName, true, '_')
    s"${camelizedParentType}${camelizedPropName}"
  }

  private def generatePkgSelect(pkg: String) = {
    val packageComponents = pkg.split("\\.").toList
    val firstSelect = q"${Term.Name(packageComponents(0))}.${Term.Name(packageComponents(1))}"
    packageComponents.tail.tail.foldLeft(firstSelect) { (a, b) =>
      q"${a}.${Term.Name(b)}"
    }
  }

}
