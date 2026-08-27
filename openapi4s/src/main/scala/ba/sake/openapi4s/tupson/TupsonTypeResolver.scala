package ba.sake.openapi4s
package tupson

import scala.meta._
import scala.meta.dialects.Scala34
import ba.sake.openapi4s.exceptions.UnsupportedSchemaException

object TupsonTypeResolver {

  def resolveType(
      schemaDef: SchemaDefinition,
      propertyName: Option[String],
      parentTypeName: Option[String],
      allowNullable: Boolean,
      context: String,
      fallbackAnyType: Type
  ): Type = schemaDef match {
    case SchemaDefinition.Opt(tpe) =>
      val core = resolveType(tpe, propertyName, parentTypeName, allowNullable = allowNullable, context, fallbackAnyType)
      if (allowNullable) t"Option[$core]" else core
    case arr: SchemaDefinition.Arr =>
      val core =
        resolveType(arr.schema, propertyName, parentTypeName, allowNullable = allowNullable, context, fallbackAnyType)
      if (arr.uniqueItems) t"Set[$core]" else t"Seq[$core]"
    case obj: SchemaDefinition.Obj          => namedTupleType(obj, context, fallbackAnyType)
    case e: SchemaDefinition.Enum           => literalUnionType(e.values.map(v => Lit.String(v): Lit))
    case e: SchemaDefinition.EnumLiterals   => literalUnionType(e.values.map(toLit))
    case c: SchemaDefinition.Const          => toLit(c.value)
    case SchemaDefinition.OneOf(schemas, _) => unionType(schemas, context, fallbackAnyType)
    case SchemaDefinition.AnyOf(schemas)    => unionType(schemas, context, fallbackAnyType)
    case SchemaDefinition.MapObj(valueSchemaOpt) =>
      valueSchemaOpt match {
        case Some(vs) =>
          t"Map[String, ${resolveType(vs, propertyName, parentTypeName, allowNullable = allowNullable, context, fallbackAnyType)}]"
        case None => t"Map[String, ${fallbackAnyType}]"
      }
    case other =>
      SchemaUtils.resolveType(other, propertyName, parentTypeName, allowNullable, context, fallbackAnyType)
  }

  private val IdentifierRegex = "[A-Za-z_][A-Za-z0-9_]*".r

  // named tuples with more fields blow up dotc/tupson derivation (github webhook unions)
  private val MaxNamedTupleFields = 15

  private def namedTupleType(obj: SchemaDefinition.Obj, context: String, fallbackAnyType: Type): Type = {
    val invalidName = obj.properties.find(p => !IdentifierRegex.matches(p.name))
    if (obj.properties.isEmpty || invalidName.isDefined || obj.properties.size > MaxNamedTupleFields) {
      println(
        s"Cannot render anonymous object as named tuple (${invalidName.map(_.name).getOrElse(s"${obj.properties.size} fields")}) [$context]. Falling back to ${fallbackAnyType.syntax}"
      )
      fallbackAnyType
    } else {
      val typedParams = obj.properties.map { p =>
        Type.TypedParam(
          // TypedParam names are not auto-backticked by the syntax printer, so sanitize explicitly
          Type.Name(ScalaIdents.sanitize(p.name)),
          resolveType(p.schema, Some(p.name), None, allowNullable = true, s"$context.${p.name}", fallbackAnyType),
          Nil
        )
      }
      Type.Tuple(typedParams)
    }
  }

  private def toLit(l: EnumLiteral): Lit = l match {
    case EnumLiteral.StrValue(v)  => Lit.String(v)
    case EnumLiteral.IntValue(v)  => Lit.Int(v)
    case EnumLiteral.LongValue(v) => Lit.Long(v)
    case EnumLiteral.NumValue(v)  => Lit.Double(v)
    case EnumLiteral.BoolValue(v) => Lit.Boolean(v)
  }

  private def literalUnionType(lits: List[Lit]): Type =
    lits.reduceLeft[Type] { (acc, lit) => t"$acc | $lit" }

  private def unionType(schemas: List[SchemaDefinition], context: String, fallbackAnyType: Type): Type =
    schemas match {
      case Nil => fallbackAnyType
      case h :: Nil => resolveType(h, None, None, allowNullable = true, context, fallbackAnyType)
      case multiple =>
        val resolved = multiple.flatMap { s =>
          try Some(resolveType(s, None, None, allowNullable = true, context, fallbackAnyType))
          catch {
            case e: UnsupportedSchemaException =>
              println(
                s"Unsupported union member [${context}]: ${e.getMessage}. Falling back to ${fallbackAnyType.syntax}"
              )
              None
          }
        }
        resolved match {
          case Nil            => fallbackAnyType
          case h :: Nil       => h
          case many =>
            many.reduceLeft[Type] { (acc, tpe) => t"$acc | $tpe" }
        }
    }
}
