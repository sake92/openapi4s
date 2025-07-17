package ba.sake.openapi4s
package sharaf

import java.nio.file.Paths
import scala.meta._
import scala.meta.dialects.Scala34
import org.apache.commons.text.CaseUtils
import ba.sake.regenesca._
import ba.sake.openapi4s.exceptions.UnsupportedSchemaException

class SharafGenerator(config: OpenApiGenerator.Config, openApiDefinition: OpenApiDefinition) extends OpenApiGenerator {

  private val merger = SourceMerger(mergeDefBodies = true)
  private val regenescaGenerator = RegenescaGenerator(merger)

  // keep track of done schemas (to avoid generating a subtype multiple times)
  private var generatedNamedSchemas = Set.empty[String]

  override def generate(): Unit = {
    println(s"Started generating Sharaf server for '${config.url}' OpenApi into '${config.baseFolder}' ...")
    val packagePath = config.basePackage.replaceAll("\\.", "/")
    val adaptedGenSourceFiles = generateSources.map { gsf =>
      gsf.copy(file = config.baseFolder.resolve(packagePath).resolve(gsf.file.toString))
    }
    regenescaGenerator.generate(adaptedGenSourceFiles)
    println(s"Finished generating Sharaf server for '${config.url}' OpenApi.")
  }

  private[sharaf] def generateSources: Seq[GeneratedFileSource] = {
    val modelsPkg = generatePkgSelect(s"${config.basePackage}.models")
    val modelImports = List[Import](
      q"import java.time.*",
      q"import java.util.UUID",
      q"import org.typelevel.jawn.ast.JValue",
      q"import ba.sake.tupson.*",
      q"import ba.sake.validson.Validator"
    )
    val modelFileSources = openApiDefinition.namedSchemaDefinitions.defs.flatMap { namedSchemaDef =>
      val namedSchemaName = namedSchemaDef.name.capitalize
      val modelSources = generateModelSources(namedSchemaDef, None)
      val allStmts = modelImports ++ modelSources
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
            resolveType(
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
          val validatorStmts = generateValidatorStmts(t"QP", validatedQPs)
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
            resolveType(
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
            val tpe = resolveType(
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
      q"import ba.sake.sharaf.*, routing.*", {
        val importer = s"${config.basePackage}.models.*".parse[Importer].get
        q"import ..${List(importer)}"
      }
    )
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

  private def generateModelSources(namedSchemaDef: SchemaDefinition.Named, superType: Option[Type]): List[Stat] = {
    val namedSchemaName = namedSchemaDef.name.capitalize
    if (generatedNamedSchemas(namedSchemaName)) return List.empty
    val typeName = Type.Name(namedSchemaName)
    val termName = Term.Name(namedSchemaName)
    val generatedModelSources = namedSchemaDef.schema match {
      case obj: SchemaDefinition.Obj =>
        val params = obj.properties.flatMap { property =>
          try {
            val propertyTpe = resolveType(
              property.schema,
              Some(property.name),
              Some(namedSchemaName),
              allowNullable = true,
              context = s"${namedSchemaName}.${property.name}",
              fallbackAnyType = t"JValue"
            )
            Some(param"${Term.Name(property.name)}: ${propertyTpe}")
          } catch {
            case e: UnsupportedSchemaException =>
              println(e.toString)
              None
          }
        }
        // enums defined in-place, we invent a new name for them..
        val adHocEnums = obj.properties.flatMap { property =>
          val enumValuesOpt = property.schema match {
            case SchemaDefinition.Enum(values, _)                                => Some(values)
            case SchemaDefinition.Opt(SchemaDefinition.Enum(values, _))          => Some(values)
            case SchemaDefinition.Arr(SchemaDefinition.Enum(values, _), _, _, _) => Some(values)
            case _                                                               => None
          }
          enumValuesOpt.flatMap { values =>
            val adhocEnumName = generateEnumName(namedSchemaName, property.name)
            val adhocEnumType = Type.Name(adhocEnumName)
            val enumCaseDefs = Defn.RepeatedEnumCase(
              List.empty,
              values.map { enumDefCaseValue =>
                Term.Name(enumDefCaseValue)
              }
            )
            Some(
              q""" enum ${adhocEnumType} derives JsonRW { ${enumCaseDefs} }"""
            )
          }
        }
        // validation
        val validatorStmts = generateValidatorStmts(typeName, obj.properties.map(p => (p.name, p.schema)))
        val classDefinition = superType match {
          case Some(st) =>
            val extendsInit = init"${st}()"
            q""" case class ${typeName}( ..${Term.ParamClause(params)} ) extends ${extendsInit}"""
          case None => q""" case class ${typeName}( ..${Term.ParamClause(params)} ) derives JsonRW """
        }
        val modelDefStats = List(classDefinition) ++
          Option.when(validatorStmts.nonEmpty)(q""" object ${termName} { ..${validatorStmts} } """).toList

        modelDefStats ++ adHocEnums
      case enumDef: SchemaDefinition.Enum =>
        val enumCaseDefs = Defn.RepeatedEnumCase(
          List.empty,
          enumDef.values.map { enumDefCaseValue =>
            Term.Name(enumDefCaseValue)
          }
        )
        List(
          q"""enum ${typeName} derives JsonRW { ${enumCaseDefs} } """
        )
      case _: SchemaDefinition.Arr =>
        // TODO type alias ???
        List.empty
      case oneOfSchema: SchemaDefinition.OneOf =>
        val oneOfCases = oneOfSchema.schemas.flatMap {
          case SchemaDefinition.Ref(refName) =>
            openApiDefinition.namedSchemaDefinitions.defs.find(_.name == refName) match {
              case Some(referencedNamedSchema) => generateModelSources(referencedNamedSchema, Some(typeName))
              case None =>
                println(s"Non-existing sub-schema type: '${refName}' [${namedSchemaName}}]")
                None
            }
          case other =>
            println(s"Unsupported oneOf sub-schema type: '${other.getClass}' [${namedSchemaName}]")
            None
        }
        List(
          q"""
          @discriminator(${Lit.String(oneOfSchema.discriminatorPropertyName)})
          sealed trait ${typeName} derives JsonRW
          """,
          q"""  object ${termName} { ..${oneOfCases} } """
        )
      case allOfSchema: SchemaDefinition.AllOf =>
        val allOfCases: List[SchemaDefinition] = allOfSchema.schemas.flatMap {
          case SchemaDefinition.Ref(refName) =>
            openApiDefinition.namedSchemaDefinitions.defs.find(_.name == refName).map(_.schema)
          case obj: SchemaDefinition.Obj => Some(obj)
          case other =>
            println(s"Unsupported allOf sub-schema type: '${other.getClass}' [${namedSchemaName}]")
            None
        }
        val mergedSchemasProps: List[SchemaProperty] = allOfCases.flatMap {
          case SchemaDefinition.Obj(props) => props
          case other => 
            println(s"Unsupported allOf sub-schema type: '${other.getClass}' [${namedSchemaName}]")
            List.empty
        }
        
        generateModelSources(SchemaDefinition.Named(namedSchemaName, SchemaDefinition.Obj(mergedSchemasProps)), superType)
    }
    generatedNamedSchemas += namedSchemaName
    generatedModelSources
  }

  private def resolveType(
      schemaDef: SchemaDefinition,
      propertyName: Option[String],
      parentTypeName: Option[String],
      // e.g. path enum cannot be null..
      allowNullable: Boolean,
      context: String,
      fallbackAnyType: Type
  ): Type = schemaDef match {
    case _: SchemaDefinition.Str         => t"String"
    case _: SchemaDefinition.Password    => t"String"
    case _: SchemaDefinition.Email       => t"String"
    case _: SchemaDefinition.Base64Bytes => t"String" // TODO use some kind of newtype.. ?
    case _: SchemaDefinition.Int32       => t"Int"
    case _: SchemaDefinition.Int64       => t"Long"
    case _: SchemaDefinition.Num32       => t"Float"
    case _: SchemaDefinition.Num64       => t"Double"
    case _: SchemaDefinition.Bool        => t"Boolean"
    case _: SchemaDefinition.Uuid        => t"UUID"
    case _: SchemaDefinition.Date        => t"LocalDate"
    case _: SchemaDefinition.DateTime    => t"Instant"
    case SchemaDefinition.Opt(tpe) =>
      val coreTpe =
        resolveType(tpe, propertyName, parentTypeName, allowNullable = allowNullable, context, fallbackAnyType)
      if (allowNullable) t"Option[${coreTpe}]"
      else coreTpe
    case arr: SchemaDefinition.Arr =>
      val coreTpe =
        resolveType(arr.schema, propertyName, parentTypeName, allowNullable = allowNullable, context, fallbackAnyType)
      if (arr.uniqueItems) t"Set[${coreTpe}]"
      else t"Seq[${coreTpe}]"
    case SchemaDefinition.Enum(_, _) =>
      (parentTypeName.zip(propertyName)) match {
        case Some((parentType, propName)) =>
          Type.Name(generateEnumName(parentType, propName))
        case _ =>
          throw new UnsupportedSchemaException(s"Cannot make up an ad hoc type for unnamed 'enum' [${context}]")
      }
    case SchemaDefinition.Ref(name)      => Type.Name(name)
    case SchemaDefinition.Named(name, _) => Type.Name(name)
    case SchemaDefinition.Obj(_) =>
      throw new UnsupportedSchemaException(s"Cannot make up an ad hoc type for 'object' [${context}]")
    case _: SchemaDefinition.OneOf =>
      throw new UnsupportedSchemaException(s"Cannot make up an ad hoc type for 'oneOf' [${context}]")
    case _: SchemaDefinition.Unknown => fallbackAnyType
  }

  private def generateValidatorStmts(typeName: Type, properties: List[(String, SchemaDefinition)]): List[Stat] = {
    val validationCalls = properties.flatMap { case (name, schema) =>
      val propName = Term.Name(name)
      schema match {
        case int: SchemaDefinition.Int32 =>
          List(
            int.minimum.map { min => "min" -> List(q"_.${propName}", Lit.Int(min)) },
            int.maximum.map { max => "max" -> List(q"_.${propName}", Lit.Int(max)) }
          ).flatten
        case long: SchemaDefinition.Int64 =>
          List(
            long.minimum.map { min => "min" -> List(q"_.${propName}", Lit.Long(min)) },
            long.maximum.map { max => "max" -> List(q"_.${propName}", Lit.Long(max)) }
          ).flatten
        case float: SchemaDefinition.Num32 =>
          List(
            float.minimum.map { min => "min" -> List(q"_.${propName}", Lit.Float(min)) },
            float.maximum.map { max => "max" -> List(q"_.${propName}", Lit.Float(max)) }
          ).flatten
        case double: SchemaDefinition.Num64 =>
          List(
            double.minimum.map { min => "min" -> List(q"_.${propName}", Lit.Double(min)) },
            double.maximum.map { max => "max" -> List(q"_.${propName}", Lit.Double(max)) }
          ).flatten
        case str: SchemaDefinition.Str =>
          List(
            str.minLength.map { min => "minLength" -> List(q"_.${propName}", Lit.Int(min)) },
            str.maxLength.map { max => "maxLength" -> List(q"_.${propName}", Lit.Int(max)) },
            str.pattern.map { pattern => "matches" -> List(q"_.${propName}", Lit.String(pattern)) }
          ).flatten
        case arr: SchemaDefinition.Arr =>
          List(
            arr.minItems.map { min => "minItems" -> List(q"_.${propName}", Lit.Int(min)) },
            arr.maxItems.map { max => "maxItems" -> List(q"_.${propName}", Lit.Int(max)) }
          ).flatten
        case _ => List.empty
      }
    }
    Option
      .when(validationCalls.nonEmpty) {
        val init = q"Validator.derived[${typeName}]"
        val body = validationCalls.foldLeft(init: Term) { case (a, (funName, funArgs)) =>
          Term.Apply(
            Term.Select(a, Term.Name(funName)),
            Term.ArgClause(funArgs)
          )
        }
        q"given Validator[${typeName}] = ${body}"
      }
      .toList
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
