package ba.sake.openapi4s

import scala.jdk.CollectionConverters._
import io.swagger.v3.oas.models.media._
import ba.sake.openapi4s.exceptions.UnsupportedSchemaDefinitionException

// https://swagger.io/docs/specification/v3_0/data-models/data-types
// https://github.com/swagger-api/swagger-core/tree/v2.2.27/modules/swagger-models/src/main/java/io/swagger/v3/oas/models/media
class SchemaDefinitionResolver {

  def resolveNamedSchemas(
      schemas: Map[String, Schema[?]]
  ): NamedSchemaDefinitions = {
    val schemaDefs = schemas.flatMap { case (schemaKey, schema) =>
      val schemaDef = resolveSchema(schema, schemaKey)
      schemaDef match {
        case obj: SchemaDefinition.Obj     => Some(SchemaDefinition.Named(schemaKey, obj))
        case enm: SchemaDefinition.Enum    => Some(SchemaDefinition.Named(schemaKey, enm))
        case arr: SchemaDefinition.Arr     => Some(SchemaDefinition.Named(schemaKey, arr))
        case oneOf: SchemaDefinition.OneOf => Some(SchemaDefinition.Named(schemaKey, oneOf))
        case allOf: SchemaDefinition.AllOf => Some(SchemaDefinition.Named(schemaKey, allOf))
        case other =>
          println(
            s"Unsupported named schema at ${schemaKey} [${other}]. Skipping the model. This may cause cascading failures!!"
          )
          None
      }
    }.toList
    NamedSchemaDefinitions(schemaDefs)
  }

  def resolveSchema(schema: Schema[?], context: String): SchemaDefinition = {
    val defaultValue = Option(schema.getDefault).map(_.toString)
    val baseSchemaDef: SchemaDefinition = schema match {
      case _: StringSchema | _: PasswordSchema | _: EmailSchema | _: ByteArraySchema | _: UUIDSchema | _: DateSchema |
          _: DateTimeSchema =>
        getStringSchema(schema)
      case _: IntegerSchema =>
        getIntegerSchema(schema)
      case _: NumberSchema =>
        getNumberSchema(schema)
      case _: BooleanSchema =>
        SchemaDefinition.Bool(defaultValue)
      case _: ArraySchema =>
        getArraySchema(schema, context)
      case _: ObjectSchema =>
        getObjectSchema(schema, context)
      case _: ComposedSchema =>
        getComposedSchema(schema, context).getOrElse {
          println(s"Unsupported composed schema at ${context}. Returning Unknown schema.")
          SchemaDefinition.Unknown()
        }
      // OpenApi 3.1 complicates this a bit
      case jsonSchema: JsonSchema =>
        val types = Option(jsonSchema.getTypes).map(_.asScala).getOrElse(Set.empty)
        // we only handle the first type...
        types.headOption match {
          case Some(schemaType) =>
            schemaType match {
              case "string" =>
                getStringSchema(schema)
              case "integer" =>
                getIntegerSchema(schema)
              case "number" =>
                getNumberSchema(schema)
              case "boolean" =>
                SchemaDefinition.Bool(defaultValue)
              case "object" =>
                getObjectSchema(schema, context)
              case "array" =>
                getArraySchema(schema, context)
              case "null" =>
                throw new UnsupportedSchemaDefinitionException(s"Null is unsupported [${context}]")
            }
          case None =>
            getComposedSchema(schema, context)
              .orElse {
                Option(schema.get$ref)
                  .map { refName =>
                    val refTpeName = refName.split("/").last
                    SchemaDefinition.Ref(refTpeName)
                  }
              }
              .getOrElse {
                println(s"Unknown type at ${context}")
                SchemaDefinition.Unknown()
              }
        }
      case _ =>
        Option(schema.get$ref)
          .map { refName =>
            val refTpeName = refName.split("/").last
            SchemaDefinition.Ref(refTpeName)
          }
          .getOrElse {
            println(s"Unknown type at ${context}")
            SchemaDefinition.Unknown()
          }

      // TODO  file, with multipart forms!...
      // case _: BinarySchema =>
    }
    val nullable = schema.getNullable
    if (nullable) SchemaDefinition.Opt(baseSchemaDef)
    else baseSchemaDef
  }

  private def getStringSchema(schema: Schema[?]): SchemaDefinition = {
    val defaultValue = Option(schema.getDefault).map(_.toString)
    val format = Option(schema.getFormat)
    Option(schema.getEnum) match {
      case Some(enumSchema) =>
        val values = enumSchema.asScala.toList.map(_.toString)
        SchemaDefinition.Enum(values, defaultValue)
      case None =>
        val minLength = Option(schema.getMinLength).map(_.intValue)
        val maxLength = Option(schema.getMaxLength).map(_.intValue)
        val pattern = Option(schema.getPattern)
        format.getOrElse("") match {
          case "password" =>
            SchemaDefinition.Password(defaultValue, minLength = minLength, maxLength = maxLength, pattern = pattern)
          case "email" =>
            SchemaDefinition.Email(defaultValue, minLength = minLength, maxLength = maxLength)
          case "byte" =>
            SchemaDefinition.Base64Bytes(defaultValue)
          case "uuid" =>
            SchemaDefinition.Uuid(defaultValue)
          case "date" =>
            SchemaDefinition.Date(defaultValue)
          case "date-time" =>
            SchemaDefinition.DateTime(defaultValue)
          case _ =>
            SchemaDefinition.Str(defaultValue, minLength = minLength, maxLength = maxLength, pattern = pattern)
        }
    }
  }

  private def getIntegerSchema(schema: Schema[?]): SchemaDefinition = {
    val defaultValue = Option(schema.getDefault).map(_.toString)
    val min = Option(schema.getMinimum)
    val max = Option(schema.getMaximum)
    if (schema.getFormat == "int32")
      SchemaDefinition.Int32(defaultValue, minimum = min.map(_.intValue), maximum = max.map(_.intValue))
    else
      SchemaDefinition.Int64(defaultValue, minimum = min.map(_.longValue), maximum = max.map(_.longValue))
  }

  private def getNumberSchema(schema: Schema[?]): SchemaDefinition = {
    val defaultValue = Option(schema.getDefault).map(_.toString)
    val min = Option(schema.getMinimum)
    val max = Option(schema.getMaximum)
    if (schema.getFormat == "float")
      SchemaDefinition.Num32(defaultValue, minimum = min.map(_.floatValue), maximum = max.map(_.floatValue))
    else SchemaDefinition.Num64(defaultValue, minimum = min.map(_.doubleValue), maximum = max.map(_.doubleValue))
  }

  private def getObjectSchema(schema: Schema[?], context: String): SchemaDefinition.Obj = {
    val requiredProperties = Option(schema.getRequired).map(_.asScala.toSet).getOrElse(Set.empty)
    val properties = Option(schema.getProperties)
      .map(_.asScala)
      .getOrElse(List.empty)
      .map { case (propertyKey, property) =>
        val coreSchema = resolveSchema(property, s"${context}.${propertyKey}")
        val schema = if (requiredProperties(propertyKey)) coreSchema else SchemaDefinition.Opt(coreSchema)
        SchemaProperty(propertyKey, schema)
      }
      .toList
      .distinct
    SchemaDefinition.Obj(properties)
  }

  private def getComposedSchema(schema: Schema[?], context: String): Option[SchemaDefinition] = {
    Option(schema.getOneOf())
      .map(_ => getOneOfSchema(schema, context))
      .orElse(Option(schema.getAllOf()).map(_ => getAllOfSchema(schema, context)))
  }

  private def getOneOfSchema(schema: Schema[?], context: String): SchemaDefinition.OneOf = {
    val schemas =
      Option(schema.getOneOf).map(_.asScala).getOrElse(List.empty).map(s => resolveSchema(s, context)).toList
    val discriminatorPropertyName = Option(schema.getDiscriminator).map(_.getPropertyName).getOrElse {
      println(s"The oneOf schema does not have a discriminator property [${context}]. Using '@type' as default")
      "@type"
    }
    SchemaDefinition.OneOf(schemas, discriminatorPropertyName = discriminatorPropertyName)
  }

  private def getAllOfSchema(schema: Schema[?], context: String): SchemaDefinition.AllOf = {
    val schemas =
      Option(schema.getAllOf).map(_.asScala).getOrElse(List.empty).map(s => resolveSchema(s, context)).toList
    SchemaDefinition.AllOf(schemas)
  }

  private def getArraySchema(schema: Schema[?], context: String): SchemaDefinition.Arr = {
    val arrayItemsSchema = schema.getItems
    val arrayItemsType = resolveSchema(arrayItemsSchema, context)
    val uniqueItems = Option(schema.getUniqueItems).exists(_.booleanValue)
    val minItems = Option(schema.getMinItems).map(_.intValue)
    val maxItems = Option(schema.getMaxItems).map(_.intValue)
    SchemaDefinition.Arr(arrayItemsType, minItems = minItems, maxItems = maxItems, uniqueItems = uniqueItems)
  }

}
