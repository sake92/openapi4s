package ba.sake.openapi4s
package tupson

import scala.meta.*
import ba.sake.openapi4s.exceptions.UnsupportedSchemaException

class TupsonModelGenerator(openApiDefinition: OpenApiDefinition) {

  // keep track of done schemas (to avoid generating a subtype multiple times)
  private var generatedNamedSchemas = Set.empty[String]

  def generateModelSources(namedSchemaDef: SchemaDefinition.Named, superType: Option[Type]): List[Stat] = {
    val namedSchemaName = namedSchemaDef.name.capitalize
    if (generatedNamedSchemas(namedSchemaName)) return List.empty
    val typeName = Type.Name(namedSchemaName)
    val termName = Term.Name(namedSchemaName)
    val generatedModelSources = namedSchemaDef.schema match {
      case obj: SchemaDefinition.Obj =>
        val params = obj.properties.flatMap { property =>
          try {
            val propertyTpe = SchemaUtils.resolveType(
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
            val adhocEnumName = SchemaUtils.generateEnumName(namedSchemaName, property.name)
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
        val validatorStmts = SchemaUtils.generateValidatorStmts(typeName, obj.properties.map(p => (p.name, p.schema)))
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
}
