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
            val propertyTpe = TupsonTypeResolver.resolveType(
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

        modelDefStats
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
      case oneOfSchema: SchemaDefinition.OneOf if oneOfSchema.discriminatorPropertyName.isDefined =>
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
        // subtypes are generated at package level, so that they can be referenced
        // by other schemas too (e.g. union type aliases)
        List(
          q"""
          @discriminator(${Lit.String(oneOfSchema.discriminatorPropertyName.get)})
          sealed trait ${typeName} derives JsonRW
          """
        ) ++ oneOfCases
      case aliasSchema: NameableSchemaDefinition
          if isTypeAliasSchema(aliasSchema) =>
        // named enums with raw values, consts, maps, ad hoc unions -> type aliases
        List(q"type ${typeName} = ${TupsonTypeResolver.resolveType(aliasSchema, None, None, allowNullable = true, namedSchemaName, t"JValue")}")
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
      case _: SchemaDefinition.Arr =>
        // TODO type alias ???
        List.empty
      case other =>
        println(s"Unsupported named schema type for tupson: '${other.getClass.getSimpleName}' [${namedSchemaName}]")
        List.empty
    }
    generatedNamedSchemas += namedSchemaName
    generatedModelSources
  }

  private def isTypeAliasSchema(s: NameableSchemaDefinition): Boolean = s match {
    case _: SchemaDefinition.EnumLiterals => true
    case _: SchemaDefinition.Const        => true
    case _: SchemaDefinition.MapObj       => true
    case _: SchemaDefinition.OneOf        => true // without discriminator
    case _: SchemaDefinition.AnyOf        => true
    case _                                => false
  }

  private def generatePkgSelect(pkg: String) = {
    val packageComponents = pkg.split("\\.").toList
    val firstSelect = q"${Term.Name(packageComponents(0))}.${Term.Name(packageComponents(1))}"
    packageComponents.tail.tail.foldLeft(firstSelect) { (a, b) =>
      q"${a}.${Term.Name(b)}"
    }
  }
}
