package ba.sake.openapi4s

import scala.jdk.CollectionConverters._
import io.swagger.parser.OpenAPIParser
import io.swagger.v3.oas.models.media._

class UnsupportedSchemaTypeException(msg: String) extends RuntimeException(msg)

// https://swagger.io/docs/specification/v3_0/data-models/data-types
class SchemaDefinitionResolver {

  def resolveNamedSchemas(
      schemas: Map[String, Schema[?]]
  ): NamedSchemaDefinitions = {
    val schemaDefs = schemas.map { case (schemaKey, schema) =>
      // must be an Obj !
      val schemaDef = resolveSchema(schema)
      if (schemaDef.isInstanceOf[SchemaDefinition.Obj])
        SchemaDefinition.Named(
          schemaKey,
          schemaDef.asInstanceOf[SchemaDefinition.Obj]
        )
      else throw new UnsupportedSchemaTypeException(s"${schemaDef.getClass}")
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
          s"Unsupported schema type: ${schema.getClass()}"
        )
      }
    val nullable = schema.getNullable()
    if (nullable) SchemaDefinition.Opt(baseType)
    else baseType
  }

  // https://github.com/swagger-api/swagger-core/tree/v2.2.27/modules/swagger-models/src/main/java/io/swagger/v3/oas/models/media
  private def resolveScalarishType(
      schema: Schema[?]
  ): Option[SchemaDefinition] = {
    val pf: PartialFunction[Schema[?], SchemaDefinition] = {
      case _: StringSchema =>
        if (schema.getEnum != null) {
          val values = schema.getEnum.asScala.toSeq.map(_.toString)
          SchemaDefinition.Enum(values)
        } else {
          SchemaDefinition.Str()
        }
      // TODO pattern/regex
      case _: PasswordSchema => // masked
        SchemaDefinition.Str()
      case _: EmailSchema =>
        SchemaDefinition.Str()
      case _: ByteArraySchema => // base64 encoded
        // TODO just add a comment, or special type !??
        SchemaDefinition.Str()
      case _: IntegerSchema =>
        // TODO min/max
        if (schema.getFormat() == "int32") SchemaDefinition.Int32()
        else SchemaDefinition.Int64()
      case _: NumberSchema =>
        SchemaDefinition.Double() // TODO Float
      case _: BooleanSchema =>
        SchemaDefinition.Bool()
      case _: UUIDSchema =>
        SchemaDefinition.UUID()
      case _: DateSchema =>
        SchemaDefinition.Date()
      case _: DateTimeSchema =>
        SchemaDefinition.DateTime()
      // TODO  file, with multipart forms!...
      // case _: BinarySchema =>

    }
    pf.lift(schema)
  }

  private def resolveArrType(
      schema: Schema[?]
  ): Option[SchemaDefinition.Arr] = {
    val pf: PartialFunction[Schema[?], SchemaDefinition.Arr] = {
      // TODO minItems/maxItems, uniqueItems->Set??
      case _: ArraySchema =>
        val arrayItemsSchema = schema.getItems()
        val arrayItemsType = resolveSchema(arrayItemsSchema)
        SchemaDefinition.Arr(arrayItemsType)
    }
    pf.lift(schema)
  }

  private def resolveObjType(
      schema: Schema[?]
  ): Option[SchemaDefinition.Obj] = {
    val pf: PartialFunction[Schema[?], SchemaDefinition.Obj] = { case _: ObjectSchema =>
      val requiredProperties = Option(schema.getRequired).map(_.asScala.toSet).getOrElse(Set.empty)
      val properties = schema
        .getProperties()
        .asScala
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
