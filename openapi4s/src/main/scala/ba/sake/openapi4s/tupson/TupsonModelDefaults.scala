package ba.sake.openapi4s
package tupson

import scala.meta._
import scala.meta.dialects.Scala34

/** Missing optional scalars can use constructor defaults; explicit null stays None. */
private[tupson] object TupsonModelDefaults {
  def optionalDefault(schema: SchemaDefinition): Option[Term] = schema match {
    case SchemaDefinition.Opt(SchemaDefinition.Int32(Some(value), _, _)) => Some(q"Some(${Lit.Int(value.toInt)})")
    case SchemaDefinition.Opt(SchemaDefinition.Int64(Some(value), _, _)) => Some(q"Some(${Lit.Long(value.toLong)})")
    case SchemaDefinition.Opt(SchemaDefinition.Num32(Some(value), _, _)) => Some(q"Some(${Lit.Float(value.toFloat)})")
    case SchemaDefinition.Opt(SchemaDefinition.Num64(Some(value), _, _)) => Some(q"Some(${Lit.Double(value.toDouble)})")
    case SchemaDefinition.Opt(SchemaDefinition.Str(Some(value), _, _, _)) => Some(q"Some(${Lit.String(value)})")
    case SchemaDefinition.Opt(SchemaDefinition.Bool(Some(value))) => Some(q"Some(${Lit.Boolean(value.toBoolean)})")
    case _                                                        => None
  }
}
