package ba.sake.openapi4s

case class NamedSchemaDefinitions(
    defs: Seq[SchemaDefinition.Named]
)

sealed abstract class SchemaDefinition

trait NameableSchemaDefinition extends SchemaDefinition // marker trait

object SchemaDefinition {
  case class Str(default: Option[String], minLength: Option[Int], maxLength: Option[Int], pattern: Option[String])
      extends SchemaDefinition
  case class Password(default: Option[String], minLength: Option[Int], maxLength: Option[Int], pattern: Option[String])
      extends SchemaDefinition
  case class Email(default: Option[String], minLength: Option[Int], maxLength: Option[Int]) extends SchemaDefinition
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
  case class Ref(name: String) extends SchemaDefinition
  case class Obj(properties: List[SchemaProperty]) extends NameableSchemaDefinition
  case class Enum(values: List[String], default: Option[String]) extends NameableSchemaDefinition
  case class EnumLiterals(values: List[EnumLiteral], default: Option[String]) extends NameableSchemaDefinition
  case class Const(value: EnumLiteral, default: Option[String]) extends NameableSchemaDefinition
  case class Arr(schema: SchemaDefinition, minItems: Option[Int], maxItems: Option[Int], uniqueItems: Boolean)
      extends NameableSchemaDefinition
  case class OneOf(schemas: List[SchemaDefinition], discriminatorPropertyName: Option[String])
      extends NameableSchemaDefinition
  case class AnyOf(schemas: List[SchemaDefinition]) extends NameableSchemaDefinition
  case class AllOf(schemas: List[SchemaDefinition]) extends NameableSchemaDefinition
  case class MapObj(valueSchema: Option[SchemaDefinition]) extends NameableSchemaDefinition

  // invented here
  case class Named(name: String, schema: NameableSchemaDefinition) extends SchemaDefinition
  case class Unknown() extends SchemaDefinition
}

sealed abstract class EnumLiteral
object EnumLiteral {
  case class StrValue(value: String) extends EnumLiteral
  case class IntValue(value: Int) extends EnumLiteral
  case class LongValue(value: Long) extends EnumLiteral
  case class NumValue(value: Double) extends EnumLiteral
  case class BoolValue(value: Boolean) extends EnumLiteral
}

case class SchemaProperty(name: String, schema: SchemaDefinition)
