package ba.sake.openapi4s
package circe

import com.typesafe.scalalogging.LazyLogging

import java.nio.file.Paths
import scala.meta._
import scala.meta.dialects.Scala34
import ba.sake.regenesca._
import ba.sake.openapi4s.exceptions.UnsupportedSchemaException

class CirceModelGenerator(
    config: OpenApiWriter.Config,
    openApiDefinition: OpenApiDefinition,
    validationTypeMap: Map[String, Map[String, String]] = Map.empty
) extends OpenApiGenerator
    with LazyLogging {

  // keep track of done schemas (to avoid generating a subtype multiple times)
  private var generatedNamedSchemas = Set.empty[String]

  override def generate(): Seq[GeneratedFileSource] = {
    val modelsPkg = generatePkgSelect(s"${config.basePackage}.models")
    val baseImports = List[Import](
      q"import java.time.*",
      q"import java.util.UUID",
      q"import io.circe.{Codec, Json}",
      q"import io.circe.derivation.{Configuration, ConfiguredCodec, ConfiguredEnumCodec}"
    )
    val ironImport =
      if (validationTypeMap.nonEmpty)
        List[Import](
          q"import io.github.iltotore.iron.circe.given"
        )
      else List.empty
    val modelImports = baseImports ++ ironImport
    val generatedFileNames = scala.collection.mutable.Set.empty[String]
    val modelFileSources = openApiDefinition.namedSchemaDefinitions.defs.flatMap { namedSchemaDef =>
      val namedSchemaName = namedSchemaDef.name.capitalize
      if (!generatedFileNames.add(namedSchemaName)) {
        logger.warn(s"Duplicate model file name '${namedSchemaName}'. Skipping the model.")
        None
      } else {
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
    }
    modelFileSources
  }

  private def generateModelSources(namedSchemaDef: SchemaDefinition.Named, superType: Option[Type]): List[Stat] = {
    val namedSchemaName = namedSchemaDef.name.capitalize
    if (generatedNamedSchemas(namedSchemaName)) return List.empty
    val typeName = ScalaIdents.typeName(namedSchemaName)
    val termName = ScalaIdents.termName(namedSchemaName)
    val normalized = SchemaNormalizer.normalize(
      namedSchemaDef.schema,
      openApiDefinition.namedSchemaDefinitions.defs.map(d => d.name -> d.schema).toMap,
      namedSchemaName
    )
    val generatedModelSources = normalized match {
      case obj: SchemaDefinition.Obj =>
        val params = obj.properties.flatMap { property =>
          try {
            val resolvedType = SchemaUtils.resolveType(
              property.schema,
              Some(property.name),
              Some(namedSchemaName),
              allowNullable = true,
              context = s"${namedSchemaName}.${property.name}",
              fallbackAnyType = t"Json"
            )
            val propertyTpe = validationTypeMap
              .get(namedSchemaName)
              .flatMap(_.get(property.name))
              .map { newtypeName =>
                property.schema match {
                  case SchemaDefinition.Opt(_) => t"Option[${Type.Name(newtypeName)}]"
                  case _                       => Type.Name(newtypeName)
                }
              }
              .getOrElse(resolvedType)
            Some(param"${ScalaIdents.termName(property.name)}: ${propertyTpe}")
          } catch {
            case e: UnsupportedSchemaException =>
              logger.warn(e.toString)
              None
          }
        }
        // enums defined in-place, we invent a new name for them..
        val adHocEnums = obj.properties.flatMap { property =>
          SchemaUtils
            .enumValuesOf(property.schema)
            .map { values =>
              val adhocEnumName = SchemaUtils.generateEnumName(namedSchemaName, property.name)
              val adhocEnumType = Type.Name(adhocEnumName)
              val adhocEnumTerm = Term.Name(adhocEnumName)
              val enumCaseDefs = Defn.RepeatedEnumCase(
                List.empty,
                values.map { enumDefCaseValue =>
                  ScalaIdents.termName(enumDefCaseValue)
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
            ScalaIdents.termName(enumDefCaseValue)
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
      case mapObj: SchemaDefinition.MapObj =>
        // maps are natively supported by circe
        val aliasTpe = SchemaUtils.resolveType(
          mapObj,
          None,
          None,
          allowNullable = true,
          context = namedSchemaName,
          fallbackAnyType = t"Json"
        )
        List(q"type ${typeName} = ${aliasTpe}")
      case oneOfSchema: SchemaDefinition.OneOf =>
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
        if (oneOfCases.isEmpty) {
          logger.warn(s"OneOf without supported sub-schemas at '${namedSchemaName}'. Skipping the model.")
          List.empty
        } else {
          val baseConfiguration = q"Configuration.default.withDiscriminator(${Lit.String(
              oneOfSchema.discriminatorPropertyName.getOrElse("@type")
            )})"
          val constructorNames = oneOfSchema.schemas.collect { case SchemaDefinition.Ref(refName) =>
            val wireNames = oneOfSchema.discriminatorMapping.toList.collect {
              case (wireName, target) if target == refName || target == s"#/components/schemas/$refName" => wireName
            }.sorted
            if (wireNames.size > 1) {
              throw new UnsupportedSchemaException(
                s"Multiple discriminator values for '$refName' are not supported by Circe constructor-name mapping [$namedSchemaName]"
              )
            }
            refName.capitalize -> wireNames.headOption.getOrElse(refName)
          }
          val renamedConstructors = constructorNames.filter { case (name, wireName) => name != wireName }
          val configuration =
            if (renamedConstructors.isEmpty) baseConfiguration
            else {
              val cases = renamedConstructors.map { case (name, wireName) =>
                Case(Lit.String(name), None, Lit.String(wireName))
              } :+ Case(Pat.Var(Term.Name("name")), None, q"name")
              q"$baseConfiguration.withTransformConstructorNames(${Term.PartialFunction(cases)})"
            }
          List(
            q"sealed trait ${typeName} ",
            q"""
              object ${termName} {
                given Configuration = $configuration
                given Codec[${typeName}] = ConfiguredCodec.derived
                ..${oneOfCases}
              }
            """
          )
        }
      case allOfSchema: SchemaDefinition.AllOf =>
        // merge allOf members into a single object model (same as TupsonModelGenerator)
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
      case other =>
        logger.warn(s"Unsupported named schema type for circe: '${other.getClass.getSimpleName}' [${namedSchemaName}]")
        List.empty
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
