package ba.sake.openapi4s

import scala.meta._
import scala.meta.dialects.Scala34
import org.apache.commons.text.CaseUtils
import ba.sake.openapi4s.exceptions.UnsupportedSchemaException

object SchemaUtils {

  def resolveType(
      schemaDef: SchemaDefinition,
      propertyName: Option[String],
      parentTypeName: Option[String],
      // e.g. path enum cannot be null..
      allowNullable: Boolean,
      context: String,
      fallbackAnyType: Type
  ): Type = schemaDef match {
    case _: SchemaDefinition.Str         => t"String"
    case _: SchemaDefinition.Password    => t"String"
    case _: SchemaDefinition.Email       => t"String"
    case _: SchemaDefinition.Base64Bytes => t"String" // TODO use some kind of newtype.. ?
    case _: SchemaDefinition.Int32       => t"Int"
    case _: SchemaDefinition.Int64       => t"Long"
    case _: SchemaDefinition.Num32       => t"Float"
    case _: SchemaDefinition.Num64       => t"Double"
    case _: SchemaDefinition.Bool        => t"Boolean"
    case _: SchemaDefinition.Uuid        => t"UUID"
    case _: SchemaDefinition.Date        => t"LocalDate"
    case _: SchemaDefinition.DateTime    => t"Instant"
    case SchemaDefinition.Opt(tpe) =>
      val coreTpe =
        resolveType(tpe, propertyName, parentTypeName, allowNullable = allowNullable, context, fallbackAnyType)
      if (allowNullable) t"Option[${coreTpe}]"
      else coreTpe
    case arr: SchemaDefinition.Arr =>
      val coreTpe =
        resolveType(arr.schema, propertyName, parentTypeName, allowNullable = allowNullable, context, fallbackAnyType)
      if (arr.uniqueItems) t"Set[${coreTpe}]"
      else t"Seq[${coreTpe}]"
    case SchemaDefinition.Enum(_, _) =>
      (parentTypeName.zip(propertyName)) match {
        case Some((parentType, propName)) =>
          Type.Name(generateEnumName(parentType, propName))
        case _ =>
          throw new UnsupportedSchemaException(s"Cannot make up an ad hoc type for unnamed 'enum' [${context}]")
      }
    case SchemaDefinition.Ref(name)      => Type.Name(name)
    case SchemaDefinition.Named(name, _) => Type.Name(name)
    case SchemaDefinition.Obj(_) =>
      throw new UnsupportedSchemaException(s"Cannot make up an ad hoc type for 'object' [${context}]")
    case _: SchemaDefinition.OneOf =>
      throw new UnsupportedSchemaException(s"Cannot make up an ad hoc type for 'oneOf' [${context}]")
    case _: SchemaDefinition.Unknown => fallbackAnyType
  }

  def generateEnumName(parentType: String, propName: String): String = {
    val camelizedParentType = CaseUtils.toCamelCase(parentType, true, '_')
    val camelizedPropName = CaseUtils.toCamelCase(propName, true, '_')
    s"${camelizedParentType}${camelizedPropName}"
  }

  def generateValidatorStmts(typeName: Type, properties: List[(String, SchemaDefinition)]): List[Stat] = {
    val validationCalls = properties.flatMap { case (name, schema) =>
      val propName = Term.Name(name)
      schema match {
        case int: SchemaDefinition.Int32 =>
          List(
            int.minimum.map { min => "min" -> List(q"_.${propName}", Lit.Int(min)) },
            int.maximum.map { max => "max" -> List(q"_.${propName}", Lit.Int(max)) }
          ).flatten
        case long: SchemaDefinition.Int64 =>
          List(
            long.minimum.map { min => "min" -> List(q"_.${propName}", Lit.Long(min)) },
            long.maximum.map { max => "max" -> List(q"_.${propName}", Lit.Long(max)) }
          ).flatten
        case float: SchemaDefinition.Num32 =>
          List(
            float.minimum.map { min => "min" -> List(q"_.${propName}", Lit.Float(min)) },
            float.maximum.map { max => "max" -> List(q"_.${propName}", Lit.Float(max)) }
          ).flatten
        case double: SchemaDefinition.Num64 =>
          List(
            double.minimum.map { min => "min" -> List(q"_.${propName}", Lit.Double(min)) },
            double.maximum.map { max => "max" -> List(q"_.${propName}", Lit.Double(max)) }
          ).flatten
        case str: SchemaDefinition.Str =>
          List(
            str.minLength.map { min => "minLength" -> List(q"_.${propName}", Lit.Int(min)) },
            str.maxLength.map { max => "maxLength" -> List(q"_.${propName}", Lit.Int(max)) },
            str.pattern.map { pattern => "matches" -> List(q"_.${propName}", Lit.String(pattern)) }
          ).flatten
        case arr: SchemaDefinition.Arr =>
          List(
            arr.minItems.map { min => "minItems" -> List(q"_.${propName}", Lit.Int(min)) },
            arr.maxItems.map { max => "maxItems" -> List(q"_.${propName}", Lit.Int(max)) }
          ).flatten
        case _ => List.empty
      }
    }
    Option
      .when(validationCalls.nonEmpty) {
        val init = q"Validator.derived[${typeName}]"
        val body = validationCalls.foldLeft(init: Term) { case (a, (funName, funArgs)) =>
          Term.Apply(
            Term.Select(a, Term.Name(funName)),
            Term.ArgClause(funArgs)
          )
        }
        q"given Validator[${typeName}] = ${body}"
      }
      .toList
  }
}
