package ba.sake.openapi4s.validson

import scala.meta._
import scala.meta.dialects.Scala34
import org.apache.commons.text.CaseUtils
import ba.sake.openapi4s.SchemaDefinition

object ValidsonUtils {
  def generateStms(typeName: Type, properties: List[(String, SchemaDefinition)]): List[Stat] = {
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
