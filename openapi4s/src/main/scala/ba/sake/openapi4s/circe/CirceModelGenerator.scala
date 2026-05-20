package ba.sake.openapi4s
package circe

import scala.meta._
import scala.meta.dialects.Scala34
import ba.sake.openapi4s.exceptions.UnsupportedSchemaException

class CirceModelGenerator(openApiDefinition: OpenApiDefinition) {

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
              fallbackAnyType = t"Json"
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
          enumValuesOpt
            .map { values =>
              val adhocEnumName = SchemaUtils.generateEnumName(namedSchemaName, property.name)
              val adhocEnumType = Type.Name(adhocEnumName)
              val adhocEnumTerm = Term.Name(adhocEnumName)
              val enumCaseDefs = Defn.RepeatedEnumCase(
                List.empty,
                values.map { enumDefCaseValue =>
                  Term.Name(enumDefCaseValue)
                }
              )
              List(
                q" enum ${adhocEnumType} { ${enumCaseDefs} } ",
                q"""
                object ${adhocEnumTerm} {
                  given Configuration =  Configuration.default
                  given Codec[${adhocEnumType}] = ConfiguredEnumCodec.derived
                }
              """
              )
            }
            .getOrElse(List.empty)
        }
        val classDefinition: List[Stat] = superType match {
          case Some(st) =>
            val extendsInit = init"${st}()"
            List(q"""
              case class ${typeName}( ..${Term.ParamClause(params)} ) extends ${extendsInit}
             """)
          case None =>
            List(
              q" case class ${typeName}( ..${Term.ParamClause(params)} ) ",
              q"""
                object ${termName} {
                  given Configuration =  Configuration.default
                  given Codec[${typeName}] = ConfiguredCodec.derived
                }
              """
            )
        }
        classDefinition ++ adHocEnums

      case enumDef: SchemaDefinition.Enum =>
        val enumCaseDefs = Defn.RepeatedEnumCase(
          List.empty,
          enumDef.values.map { enumDefCaseValue =>
            Term.Name(enumDefCaseValue)
          }
        )
        List(
          q" enum ${typeName} { ${enumCaseDefs} } ",
          q"""
            object ${termName} {
              given Configuration =  Configuration.default
              given Codec[${typeName}] = ConfiguredEnumCodec.derived
            }
          """
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
          q"sealed trait ${typeName} ",
          q"""
            object ${termName} {
              given Configuration =  Configuration.default.withDiscriminator(${Lit.String(
            oneOfSchema.discriminatorPropertyName
          )})
              given Codec[${typeName}] = ConfiguredCodec.derived
              ..${oneOfCases}
            }
          """
        )
    }
    generatedNamedSchemas += namedSchemaName
    generatedModelSources
  }
}
