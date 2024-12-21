package ba.sake.openapi4s

case class NamedSchemaDefinitions(
    defs: Seq[SchemaDefinition.Named]
)

trait NameableSchemaDefinition // marker trait

// TODO "default" values !
sealed abstract class SchemaDefinition
object SchemaDefinition {
  // TODO min/max etc
  case class Str() extends SchemaDefinition
  case class Int32() extends SchemaDefinition
  case class Int64() extends SchemaDefinition // Long
  case class Double() extends SchemaDefinition
  case class Bool() extends SchemaDefinition
  case class UUID() extends SchemaDefinition
  case class Date() extends SchemaDefinition
  case class DateTime() extends SchemaDefinition
  case class Obj(properties: Seq[SchemaProperty]) extends SchemaDefinition with NameableSchemaDefinition
  case class Enum(values: Seq[String]) extends SchemaDefinition with NameableSchemaDefinition
  case class Opt(tpe: SchemaDefinition) extends SchemaDefinition 
  case class Arr(tpe: SchemaDefinition) extends SchemaDefinition
  case class Ref(name: String) extends SchemaDefinition // reference to a Obj
  case class Named(name: String, schemaDef: NameableSchemaDefinition)
      extends SchemaDefinition
}

case class SchemaProperty(name: String, tpe: SchemaDefinition)
