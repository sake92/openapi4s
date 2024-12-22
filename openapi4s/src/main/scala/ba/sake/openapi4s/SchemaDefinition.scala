package ba.sake.openapi4s

case class NamedSchemaDefinitions(
    defs: Seq[SchemaDefinition.Named]
)

trait NameableSchemaDefinition // marker trait

sealed abstract class SchemaDefinition
object SchemaDefinition {
  case class Str(default: Option[String], minLength:Option[Int], maxLength:Option[Int], pattern :Option[String]) extends SchemaDefinition
  case class Base64Bytes(default: Option[String]) extends SchemaDefinition
  case class Int32(default: Option[String], maximum: Option[Int], minimum: Option[Int]) extends SchemaDefinition
  case class Int64(default: Option[String], maximum: Option[Long], minimum: Option[Long]) extends SchemaDefinition
  case class Num32(default: Option[String], maximum: Option[Float], minimum: Option[Float]) extends SchemaDefinition
  case class Num64(default: Option[String], maximum: Option[Double], minimum: Option[Double]) extends SchemaDefinition
  case class Bool(default: Option[String]) extends SchemaDefinition
  case class Uuid(default: Option[String]) extends SchemaDefinition
  case class Date(default: Option[String]) extends SchemaDefinition
  case class DateTime(default: Option[String]) extends SchemaDefinition
  case class Opt(schema: SchemaDefinition) extends SchemaDefinition
  case class Obj(properties: Seq[SchemaProperty]) extends SchemaDefinition with NameableSchemaDefinition
  case class Enum(values: Seq[String], default: Option[String]) extends SchemaDefinition with NameableSchemaDefinition
  case class Arr(schema: SchemaDefinition, minItems:Option[Int], maxItems:Option[Int],uniqueItems: Boolean) extends SchemaDefinition with NameableSchemaDefinition
  case class Ref(name: String) extends SchemaDefinition // reference to Obj
  // TODO treat is as a type alias ???
  case class Named(name: String, schema: NameableSchemaDefinition) extends SchemaDefinition
}

case class SchemaProperty(name: String, schema: SchemaDefinition)
