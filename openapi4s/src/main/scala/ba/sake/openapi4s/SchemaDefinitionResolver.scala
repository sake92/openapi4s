package ba.sake.openapi4s

import scala.jdk.CollectionConverters._
import io.swagger.parser.OpenAPIParser
import io.swagger.v3.oas.models.media._

// TODO better naming
class UnsupportedSchemaTypeException(msg: String) extends RuntimeException(msg)

// https://swagger.io/docs/specification/v3_0/data-models/data-types
class SchemaDefinitionResolver {

  def resolveNamedSchemas(
      schemas: Map[String, Schema[?]]
  ): NamedSchemaDefinitions = {
    val schemaDefs = schemas.map { case (schemaKey, schema) =>
      val schemaDef = resolveSchema(schema)
      schemaDef match {
        case obj: SchemaDefinition.Obj => SchemaDefinition.Named(schemaKey, obj)
        case arr: SchemaDefinition.Arr => SchemaDefinition.Named(schemaKey, arr)
        case _ =>
          throw new UnsupportedSchemaTypeException(s"${schema.getClass}")
      }
    }.toSeq
    NamedSchemaDefinitions(schemaDefs)
  }

  def resolveSchema(schema: Schema[?]): SchemaDefinition = {
    val baseType = resolveScalarishType(schema)
      .orElse(resolveArrType(schema))
      .orElse(resolveObjType(schema))
      .orElse {
        Option(schema.get$ref).map { refName =>
          val refTpeName = refName.split("/").last
          SchemaDefinition.Ref(refTpeName)
        }
      }
      .getOrElse {
        throw new UnsupportedSchemaTypeException(
          s"Unsupported schema type: ${schema.getClass}"
        )
      }
    val nullable = schema.getNullable
    if (nullable) SchemaDefinition.Opt(baseType)
    else baseType
  }

  // https://github.com/swagger-api/swagger-core/tree/v2.2.27/modules/swagger-models/src/main/java/io/swagger/v3/oas/models/media
  private def resolveScalarishType(
      schema: Schema[?]
  ): Option[SchemaDefinition] = {
    val defaultValue = Option(schema.getDefault).map(_.toString)
    val pf: PartialFunction[Schema[?], SchemaDefinition] = {
      case _: StringSchema =>
        if (schema.getEnum != null) {
          val values = schema.getEnum.asScala.toSeq.map(_.toString)
          SchemaDefinition.Enum(values, defaultValue)
        } else {
          val minLength = Option(schema.getMinLength).map(_.intValue)
          val maxLength = Option(schema.getMaxLength).map(_.intValue)
          val pattern = Option(schema.getPattern)
          SchemaDefinition.Str(defaultValue, minLength = minLength, maxLength = maxLength, pattern = pattern)
        }
      case _: PasswordSchema =>
        val minLength = Option(schema.getMinLength).map(_.intValue)
        val maxLength = Option(schema.getMaxLength).map(_.intValue)
        val pattern = Option(schema.getPattern)
        SchemaDefinition.Password(defaultValue, minLength = minLength, maxLength = maxLength, pattern = pattern)
      case _: EmailSchema =>
        val minLength = Option(schema.getMinLength).map(_.intValue)
        val maxLength = Option(schema.getMaxLength).map(_.intValue)
        SchemaDefinition.Email(defaultValue, minLength = minLength, maxLength = maxLength)
      case _: ByteArraySchema =>
        SchemaDefinition.Base64Bytes(defaultValue)
      case _: IntegerSchema =>
        val min = Option(schema.getMinimum)
        val max = Option(schema.getMaximum)
        if (schema.getFormat == "int32")
          SchemaDefinition.Int32(defaultValue, minimum = min.map(_.intValue), maximum = max.map(_.intValue))
        else SchemaDefinition.Int64(defaultValue, minimum = min.map(_.longValue), maximum = max.map(_.longValue))
      case _: NumberSchema =>
        val min = Option(schema.getMinimum)
        val max = Option(schema.getMaximum)
        if (schema.getFormat == "float")
          SchemaDefinition.Num32(defaultValue, minimum = min.map(_.floatValue), maximum = max.map(_.floatValue))
        else SchemaDefinition.Num64(defaultValue, minimum = min.map(_.doubleValue), maximum = max.map(_.doubleValue))
      case _: BooleanSchema =>
        SchemaDefinition.Bool(defaultValue)
      case _: UUIDSchema =>
        SchemaDefinition.Uuid(defaultValue)
      case _: DateSchema =>
        SchemaDefinition.Date(defaultValue)
      case _: DateTimeSchema =>
        SchemaDefinition.DateTime(defaultValue)
      // TODO  file, with multipart forms!...
      // case _: BinarySchema =>
    }
    pf.lift(schema)
  }

  private def resolveArrType(
      schema: Schema[?]
  ): Option[SchemaDefinition.Arr] = {
    val pf: PartialFunction[Schema[?], SchemaDefinition.Arr] = { case _: ArraySchema =>
      val arrayItemsSchema = schema.getItems
      val arrayItemsType = resolveSchema(arrayItemsSchema)
      val uniqueItems = Option(schema.getUniqueItems).exists(_.booleanValue)
      val minItems = Option(schema.getMinItems).map(_.intValue)
      val maxItems = Option(schema.getMaxItems).map(_.intValue)
      SchemaDefinition.Arr(arrayItemsType, minItems = minItems, maxItems = maxItems, uniqueItems = uniqueItems)
    }
    pf.lift(schema)
  }

  private def resolveObjType(
      schema: Schema[?]
  ): Option[SchemaDefinition.Obj] = {
    val pf: PartialFunction[Schema[?], SchemaDefinition.Obj] = { case _: ObjectSchema =>
      val requiredProperties = Option(schema.getRequired).map(_.asScala.toSet).getOrElse(Set.empty)
      //Option(schema.getOneOf)
      val properties = schema.getProperties.asScala
        .map { case (propertyKey, property) =>
          val coreSchema = resolveSchema(property)
          val schema = if (requiredProperties(propertyKey)) coreSchema else SchemaDefinition.Opt(coreSchema)
          SchemaProperty(propertyKey, schema)
        }
        .toSeq
        .distinct
      SchemaDefinition.Obj(properties)
    }
    pf.lift(schema)
  }

}
