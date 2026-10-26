package ba.sake.openapi4s

import ba.sake.openapi4s.exceptions.UnsupportedSchemaException

/** Normalizes literal refinements expressed as object intersections. Other allOf forms retain their existing generation
  * path, including recursive OpenAPI definitions.
  */
private[openapi4s] object SchemaNormalizer {
  def normalize(
      schema: SchemaDefinition,
      definitions: Map[String, SchemaDefinition],
      context: String
  ): SchemaDefinition = {
    def unsupported(detail: String): Nothing =
      throw new UnsupportedSchemaException(s"Unsupported object intersection at '$context': $detail")

    def numeric(value: EnumLiteral): Option[BigDecimal] = value match {
      case EnumLiteral.IntValue(n)                              => Some(BigDecimal(n))
      case EnumLiteral.LongValue(n)                             => Some(BigDecimal(n))
      case EnumLiteral.NumValue(n) if !n.isNaN && !n.isInfinity => Some(BigDecimal(n))
      case _                                                    => None
    }

    def accepts(schema: SchemaDefinition, value: EnumLiteral): Boolean = schema match {
      case SchemaDefinition.Str(_, min, max, pattern) =>
        value match {
          case EnumLiteral.StrValue(s) =>
            val length = s.codePointCount(0, s.length)
            min.forall(length >= _) && max.forall(length <= _) &&
            pattern.forall(p => java.util.regex.Pattern.compile(p).matcher(s).find())
          case _ => false
        }
      case SchemaDefinition.Int32(_, max, min) =>
        numeric(value).exists(n => n.isWhole && n.isValidInt && max.forall(n <= _) && min.forall(n >= _))
      case SchemaDefinition.Int64(_, max, min) =>
        numeric(value).exists(n => n.isWhole && n.isValidLong && max.forall(n <= _) && min.forall(n >= _))
      case SchemaDefinition.Num32(_, max, min) =>
        numeric(value).exists(n =>
          max.forall(m => n <= BigDecimal(m.toDouble)) && min.forall(m => n >= BigDecimal(m.toDouble))
        )
      case SchemaDefinition.Num64(_, max, min) =>
        numeric(value).exists(n => max.forall(m => n <= BigDecimal(m)) && min.forall(m => n >= BigDecimal(m)))
      case _: SchemaDefinition.Bool => value.isInstanceOf[EnumLiteral.BoolValue]
      case _                        => false
    }

    def intersect(left: SchemaDefinition, right: SchemaDefinition): SchemaDefinition = {
      if (left == right) left
      else
        (left, right) match {
          case (SchemaDefinition.Opt(a), SchemaDefinition.Opt(b)) => SchemaDefinition.optional(intersect(a, b))
          case (SchemaDefinition.Opt(a), b)                       => intersect(a, b)
          case (a, SchemaDefinition.Opt(b))                       => intersect(a, b)
          case (c: SchemaDefinition.Const, SchemaDefinition.Enum(values, _)) =>
            c.value match {
              case EnumLiteral.StrValue(value) if values.contains(value) => c
              case _                                                     => unsupported("const does not belong to enum")
            }
          case (SchemaDefinition.Enum(_, _), c: SchemaDefinition.Const) => intersect(c, left)
          case (c: SchemaDefinition.Const, SchemaDefinition.EnumLiterals(values, _)) if values.contains(c.value) => c
          case (SchemaDefinition.EnumLiterals(_, _), c: SchemaDefinition.Const) => intersect(c, left)
          case (c: SchemaDefinition.Const, scalar) if accepts(scalar, c.value)  => c
          case (scalar, c: SchemaDefinition.Const) if accepts(scalar, c.value)  => c
          case _ => unsupported(s"cannot intersect $left with $right")
        }
    }

    def properties(schema: SchemaDefinition, seen: Set[String]): List[SchemaProperty] = schema match {
      case SchemaDefinition.Ref(name) =>
        if (seen(name)) unsupported(s"cyclic reference '$name'")
        properties(definitions.getOrElse(name, unsupported(s"unresolved reference '$name'")), seen + name)
      case SchemaDefinition.Named(_, inner) => properties(inner, seen)
      case SchemaDefinition.Obj(props)      => props
      case SchemaDefinition.AllOf(members)  => merge(members.flatMap(member => properties(member, seen)))
      case other                            => unsupported(s"expected object, found $other")
    }

    def merge(props: List[SchemaProperty]): List[SchemaProperty] =
      props.foldLeft(List.empty[SchemaProperty]) { (acc, property) =>
        acc.indexWhere(_.name == property.name) match {
          case -1    => acc :+ property
          case index => acc.updated(index, property.copy(schema = intersect(acc(index).schema, property.schema)))
        }
      }

    def hasLiteralRefinement(schema: SchemaDefinition): Boolean = schema match {
      case _: SchemaDefinition.Const       => true
      case SchemaDefinition.Opt(inner)     => hasLiteralRefinement(inner)
      case SchemaDefinition.Obj(props)     => props.exists(p => hasLiteralRefinement(p.schema))
      case SchemaDefinition.AllOf(members) => members.exists(hasLiteralRefinement)
      case _                               => false
    }

    def visit(schema: SchemaDefinition): SchemaDefinition = schema match {
      case all: SchemaDefinition.AllOf if hasLiteralRefinement(all) =>
        visit(SchemaDefinition.Obj(properties(all, Set.empty)))
      case all: SchemaDefinition.AllOf => all
      case SchemaDefinition.Obj(props) => SchemaDefinition.Obj(props.map(p => p.copy(schema = visit(p.schema))))
      case SchemaDefinition.Opt(inner) => SchemaDefinition.optional(visit(inner))
      case SchemaDefinition.Arr(inner, min, max, unique) => SchemaDefinition.Arr(visit(inner), min, max, unique)
      case SchemaDefinition.AnyOf(members)               => SchemaDefinition.AnyOf(members.map(visit))
      case oneOf: SchemaDefinition.OneOf                 => oneOf.copy(schemas = oneOf.schemas.map(visit))
      case other                                         => other
    }
    visit(schema)
  }
}
