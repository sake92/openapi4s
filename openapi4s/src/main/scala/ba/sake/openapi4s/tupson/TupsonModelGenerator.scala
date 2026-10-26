package ba.sake.openapi4s
package tupson

import com.typesafe.scalalogging.LazyLogging

import java.nio.file.Paths
import scala.meta._
import scala.meta.dialects.Scala34
import ba.sake.regenesca._
import ba.sake.openapi4s.exceptions.UnsupportedSchemaException
import ba.sake.openapi4s.validson.ValidsonUtils

class TupsonModelGenerator(config: OpenApiWriter.Config, openApiDefinition: OpenApiDefinition)
    extends OpenApiGenerator
    with LazyLogging {

  // keep track of done schemas (to avoid generating a subtype multiple times)
  private var generatedNamedSchemas = Set.empty[String]

  override def generate(): Seq[GeneratedFileSource] = {
    val modelsPkg = generatePkgSelect(s"${config.basePackage}.models")
    val generatedFileNames = scala.collection.mutable.Set.empty[String]
    val modelFileSources = openApiDefinition.namedSchemaDefinitions.defs.flatMap { namedSchemaDef =>
      val namedSchemaName = namedSchemaDef.name.capitalize
      if (!generatedFileNames.add(namedSchemaName)) {
        logger.warn(s"Duplicate model file name '${namedSchemaName}'. Skipping the model.")
        None
      } else {
        val modelSources = generateModelSources(namedSchemaDef, None)
        val referencedNames = modelSources
          .flatMap(_.collect {
            case Type.Name(name) => name
            case Term.Name(name) => name
          })
          .toSet
        val importCandidates = List(
          Set("LocalDate") -> q"import java.time.LocalDate",
          Set("Instant") -> q"import java.time.Instant",
          Set("UUID") -> q"import java.util.UUID",
          Set("JValue") -> q"import org.typelevel.jawn.ast.JValue",
          Set("JsonRW", "discriminator") -> q"import ba.sake.tupson.*",
          Set("Validator") -> q"import ba.sake.validson.Validator"
        )
        val modelImports = importCandidates.collect {
          case (names, statement) if names.exists(referencedNames.contains) => statement
        }
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
    }
    modelFileSources
  }

  def generateModelSources(namedSchemaDef: SchemaDefinition.Named, superType: Option[Type]): List[Stat] = {
    val namedSchemaName = namedSchemaDef.name.capitalize
    if (generatedNamedSchemas(namedSchemaName)) return List.empty
    val typeName = ScalaIdents.typeName(namedSchemaName)
    val termName = ScalaIdents.termName(namedSchemaName)
    val normalized = SchemaNormalizer.normalize(
      namedSchemaDef.schema,
      openApiDefinition.namedSchemaDefinitions.defs.map(d => d.name -> d.schema).toMap,
      namedSchemaName
    )
    val liftedModels = scala.collection.mutable.ListBuffer.empty[SchemaDefinition.Named]
    def liftOptionalObjects(schema: SchemaDefinition, path: String): SchemaDefinition = schema match {
      case obj: SchemaDefinition.Obj if obj.properties.exists(_.schema.isInstanceOf[SchemaDefinition.Opt]) =>
        val name = s"${namedSchemaName}__${path}"
        liftedModels += SchemaDefinition.Named(name, obj)
        SchemaDefinition.Ref(name)
      case SchemaDefinition.Opt(inner)                   => SchemaDefinition.optional(liftOptionalObjects(inner, path))
      case SchemaDefinition.Arr(inner, min, max, unique) =>
        SchemaDefinition.Arr(liftOptionalObjects(inner, s"${path}__item"), min, max, unique)
      case SchemaDefinition.AnyOf(members) =>
        SchemaDefinition.AnyOf(members.zipWithIndex.map { case (member, index) =>
          liftOptionalObjects(member, s"${path}__${index}")
        })
      case other => other
    }
    val renderable = normalized match {
      case SchemaDefinition.Obj(props) =>
        SchemaDefinition.Obj(props.map(p => p.copy(schema = liftOptionalObjects(p.schema, p.name))))
      case other => other
    }
    val generatedModelSources = renderable match {
      case obj: SchemaDefinition.Obj =>
        val params = obj.properties.flatMap { property =>
          val default = TupsonModelDefaults.optionalDefault(property.schema)
          try {
            val propertyTpe = TupsonTypeResolver.resolveType(
              property.schema,
              Some(property.name),
              Some(namedSchemaName),
              allowNullable = true,
              context = s"${namedSchemaName}.${property.name}",
              fallbackAnyType = t"JValue"
            )
            val parameter = default match {
              case Some(default) => param"${ScalaIdents.termName(property.name)}: ${propertyTpe} = ${default}"
              case None          => param"${ScalaIdents.termName(property.name)}: ${propertyTpe}"
            }
            Some(parameter)
          } catch {
            case e: UnsupportedSchemaException =>
              logger.warn(e.toString)
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
            ScalaIdents.termName(enumDefCaseValue)
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
              case None                        =>
                logger.warn(s"Non-existing sub-schema type: '${refName}' [${namedSchemaName}]")
                None
            }
          case other =>
            logger.warn(s"Unsupported oneOf sub-schema type: '${other.getClass}' [${namedSchemaName}]")
            None
        }
        // subtypes are generated at package level, so that they can be referenced
        // by other schemas too (e.g. union type aliases)
        if (oneOfCases.isEmpty) {
          logger.warn(s"OneOf without supported sub-schemas at '${namedSchemaName}'. Skipping the model.")
          List.empty
        } else {
          List(
            q"""
            @discriminator(${Lit.String(oneOfSchema.discriminatorPropertyName.get)})
            sealed trait ${typeName} derives JsonRW
            """
          ) ++ oneOfCases
        }
      case aliasSchema: NameableSchemaDefinition if isTypeAliasSchema(aliasSchema) =>
        // named enums with raw values, consts, maps, ad hoc unions -> type aliases
        List(q"type ${typeName} = ${TupsonTypeResolver
            .resolveType(aliasSchema, None, None, allowNullable = true, namedSchemaName, t"JValue")}")
      case allOfSchema: SchemaDefinition.AllOf =>
        val allOfCases: List[SchemaDefinition] = allOfSchema.schemas.flatMap {
          case SchemaDefinition.Ref(refName) =>
            openApiDefinition.namedSchemaDefinitions.defs.find(_.name == refName).map(_.schema)
          case obj: SchemaDefinition.Obj => Some(obj)
          case other                     =>
            logger.warn(s"Unsupported allOf sub-schema type: '${other.getClass}' [${namedSchemaName}]")
            None
        }
        val mergedSchemasProps: List[SchemaProperty] = allOfCases
          .flatMap {
            case SchemaDefinition.Obj(props) => props
            case other                       =>
              logger.warn(s"Unsupported allOf sub-schema type: '${other.getClass}' [${namedSchemaName}]")
              List.empty
          }
          .foldLeft(List.empty[SchemaProperty]) { (acc, p) =>
            if (acc.exists(_.name == p.name)) {
              logger.warn(
                s"Duplicate property '${p.name}' after allOf merge [${namedSchemaName}]. Keeping the first occurrence."
              )
              acc
            } else acc :+ p
          }

        generateModelSources(
          SchemaDefinition.Named(namedSchemaName, SchemaDefinition.Obj(mergedSchemasProps)),
          superType
        )
      case _: SchemaDefinition.Arr =>
        // TODO type alias ???
        List.empty
      case other =>
        logger.warn(s"Unsupported named schema type for tupson: '${other.getClass.getSimpleName}' [${namedSchemaName}]")
        List.empty
    }
    generatedNamedSchemas += namedSchemaName
    generatedModelSources ++ liftedModels.toList.flatMap(model => generateModelSources(model, None))
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
