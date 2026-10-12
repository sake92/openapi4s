package ba.sake.openapi4s
package tupson

import scala.meta._
import scala.meta.dialects.Scala34
import ba.sake.openapi4s.exceptions.UnsupportedSchemaException

/** Missing optional scalars can use constructor defaults; explicit null stays None. */
private[tupson] object TupsonModelDefaults {
  def optionalDefault(schema: SchemaDefinition): Option[Term] = schema match {
    case SchemaDefinition.Opt(SchemaDefinition.Int32(Some(value), _, _)) =>
      Some(q"Some(${Lit.Int(integerDefault(value, "Int32").intValueExact())})")
    case SchemaDefinition.Opt(SchemaDefinition.Int64(Some(value), _, _)) =>
      Some(q"Some(${Lit.Long(integerDefault(value, "Int64").longValueExact())})")
    case SchemaDefinition.Opt(SchemaDefinition.Num32(Some(value), _, _)) => Some(q"Some(${Lit.Float(value.toFloat)})")
    case SchemaDefinition.Opt(SchemaDefinition.Num64(Some(value), _, _)) => Some(q"Some(${Lit.Double(value.toDouble)})")
    case SchemaDefinition.Opt(SchemaDefinition.Str(Some(value), _, _, _)) => Some(q"Some(${Lit.String(value)})")
    case SchemaDefinition.Opt(SchemaDefinition.Bool(Some(value))) => Some(q"Some(${Lit.Boolean(value.toBoolean)})")
    case _                                                        => None
  }

  private def integerDefault(value: String, target: String): java.math.BigInteger = {
    try {
      val integer = new java.math.BigDecimal(value).toBigIntegerExact
      if (target == "Int32") integer.intValueExact() else integer.longValueExact()
      integer
    } catch {
      case _: NumberFormatException | _: ArithmeticException =>
        throw new UnsupportedSchemaException(s"Invalid $target default '$value': expected a whole number in range")
    }
  }
}
