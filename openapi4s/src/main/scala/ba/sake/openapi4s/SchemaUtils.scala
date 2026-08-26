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
    case el: SchemaDefinition.EnumLiterals => enumLiteralsPlainType(el)
    case _: SchemaDefinition.Const         => fallbackAnyType
    case SchemaDefinition.Ref(name)      => Type.Name(name)
    case SchemaDefinition.Named(name, _) => Type.Name(name)
    case SchemaDefinition.Obj(_) =>
      throw new UnsupportedSchemaException(s"Cannot make up an ad hoc type for 'object' [${context}]")
    case SchemaDefinition.MapObj(valueSchemaOpt) =>
      valueSchemaOpt match {
        case Some(vs) =>
          t"Map[String, ${resolveType(vs, propertyName, parentTypeName, allowNullable = allowNullable, context, fallbackAnyType)}]"
        case None => t"Map[String, ${fallbackAnyType}]"
      }
    case _: SchemaDefinition.OneOf =>
      throw new UnsupportedSchemaException(s"Cannot make up an ad hoc type for 'oneOf' [${context}]")
    case _: SchemaDefinition.AnyOf => fallbackAnyType
    case _: SchemaDefinition.Unknown => fallbackAnyType
  }

  private def enumLiteralsPlainType(el: SchemaDefinition.EnumLiterals): Type =
    el.values.headOption
      .map {
        case _: EnumLiteral.IntValue  => t"Int"
        case _: EnumLiteral.LongValue => t"Long"
        case _: EnumLiteral.NumValue  => t"Double"
        case _: EnumLiteral.BoolValue => t"Boolean"
        case _: EnumLiteral.StrValue  => t"String"
      }
      .getOrElse(t"String")

  def generateEnumName(parentType: String, propName: String): String = {
    val camelizedParentType = CaseUtils.toCamelCase(parentType, true, '_')
    val camelizedPropName = CaseUtils.toCamelCase(propName, true, '_')
    s"${camelizedParentType}${camelizedPropName}"
  }

}
