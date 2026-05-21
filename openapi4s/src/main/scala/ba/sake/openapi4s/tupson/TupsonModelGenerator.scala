package ba.sake.openapi4s
package tupson

import java.nio.file.Paths
import scala.meta._
import scala.meta.dialects.Scala34
import ba.sake.regenesca._
import ba.sake.openapi4s.exceptions.UnsupportedSchemaException
import ba.sake.openapi4s.validson.ValidsonUtils

class TupsonModelGenerator(config: OpenApiWriter.Config, openApiDefinition: OpenApiDefinition)
    extends OpenApiGenerator {

  // keep track of done schemas (to avoid generating a subtype multiple times)
  private var generatedNamedSchemas = Set.empty[String]

  override def generate(): Seq[GeneratedFileSource] = {
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
    modelFileSources
  }

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
        val validatorStmts = ValidsonUtils.generateStms(typeName, obj.properties.map(p => (p.name, p.schema)))
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
                println(s"Non-existing sub-schema type: '${refName}' [${namedSchemaName}]")
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

        generateModelSources(
          SchemaDefinition.Named(namedSchemaName, SchemaDefinition.Obj(mergedSchemasProps)),
          superType
        )
    }
    generatedNamedSchemas += namedSchemaName
    generatedModelSources
  }

  private def generatePkgSelect(pkg: String) = {
    val packageComponents = pkg.split("\\.").toList
    val firstSelect = q"${Term.Name(packageComponents(0))}.${Term.Name(packageComponents(1))}"
    packageComponents.tail.tail.foldLeft(firstSelect) { (a, b) =>
      q"${a}.${Term.Name(b)}"
    }
  }
}
