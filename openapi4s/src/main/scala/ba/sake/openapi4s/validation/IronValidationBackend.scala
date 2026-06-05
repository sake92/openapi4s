package ba.sake.openapi4s
package validation

import java.nio.file.Paths
import scala.meta._
import scala.meta.dialects.Scala34
import ba.sake.regenesca._
import ba.sake.openapi4s.OpenApiWriter.Config

object IronValidationBackend extends ValidationBackend {
  override val id: ValidationBackendId = ValidationBackendId.Iron
  override val supportedModelIds: Set[ModelBackendId] = Set(ModelBackendId.Circe)

  override def generate(
      config: Config,
      openApiDefinition: OpenApiDefinition
  ): (Seq[GeneratedFileSource], Map[String, Map[String, String]]) = {

    // --- Pass 1: collect all constrained properties ---
    case class PropEntry(schemaName: String, propName: String, baseType: String, constraintKey: String, schemaDef: SchemaDefinition)

    val allEntries: List[PropEntry] = openApiDefinition.namedSchemaDefinitions.defs.flatMap { namedSchemaDef =>
      val schemaName = namedSchemaDef.name.capitalize
      namedSchemaDef.schema match {
        case obj: SchemaDefinition.Obj =>
          obj.properties.flatMap { prop =>
            constraintInfo(prop.schema, schemaName, prop.name).map { case (baseType, constraintKey) =>
              PropEntry(schemaName, prop.name, baseType, constraintKey, prop.schema)
            }
          }
        case _ => List.empty
      }
    }.toList

    // --- Pass 2: group by property name, resolve naming conflicts ---
    val byPropName: Map[String, List[PropEntry]] = allEntries.groupBy(_.propName)

    val typeMap = scala.collection.mutable.Map.empty[String, Map[String, String]]
    val newtypeStmts = scala.collection.mutable.ListBuffer.empty[Stat]

    byPropName.foreach { case (propName, entries) =>
      val constraintKeys = entries.map(_.constraintKey).distinct

      if (constraintKeys.size == 1) {
        // Single constraint: shared name = capitalized propName
        val typeName = propName.capitalize
        val entry = entries.head
        newtypeStmts += generateNewtypeDef(typeName, entry.baseType, entry.constraintKey)

        entries.foreach { e =>
          val sm = typeMap.getOrElse(e.schemaName, Map.empty)
          typeMap(e.schemaName) = sm + (e.propName -> typeName)
        }
      } else {
        // Different constraints: prefix with schema name
        entries.foreach { entry =>
          val typeName = s"${entry.schemaName}${entry.propName.capitalize}"
          newtypeStmts += generateNewtypeDef(typeName, entry.baseType, entry.constraintKey)

          val sm = typeMap.getOrElse(entry.schemaName, Map.empty)
          typeMap(entry.schemaName) = sm + (entry.propName -> typeName)
        }
      }
    }

    if (newtypeStmts.isEmpty) {
      (Seq.empty, Map.empty)
    } else {
      val pkgParts = config.basePackage.split("\\.").toList
      val modelsPkg = pkgParts.init.foldLeft[Term.Ref](Term.Name(pkgParts.head)) { (acc, part) =>
        Term.Select(acc, Term.Name(part))
      }
      val modelsPkgSelect = Term.Select(modelsPkg, Term.Name("models"))

      val imports = List[Import](
        q"import io.github.iltotore.iron.*",
        q"import io.github.iltotore.iron.constraint.all.*"
      )

      val source = source"""
        // generated new types with OpenApi4s
        package ${modelsPkgSelect} {
          ..${imports}
          ..${newtypeStmts.toList}
        }
      """

      val file = GeneratedFileSource(
        Paths.get("models/Newtypes.scala"),
        source
      )

      (Seq(file), typeMap.toMap)
    }
  }

  /** Get constraint info for a schema definition. Returns (baseType, constraintKey) tuple. */
  private def constraintInfo(
      schemaDef: SchemaDefinition,
      parentSchemaName: String,
      propName: String
  ): Option[(String, String)] = schemaDef match {
    case email: SchemaDefinition.Email =>
      Some(("String", buildConstraintKey("Email", email.minLength, email.maxLength, None)))
    case pwd: SchemaDefinition.Password =>
      if (pwd.minLength.isEmpty && pwd.maxLength.isEmpty && pwd.pattern.isEmpty) None
      else Some(("String", buildConstraintKey("Password", pwd.minLength, pwd.maxLength, pwd.pattern)))
    case str: SchemaDefinition.Str =>
      if (str.minLength.isEmpty && str.maxLength.isEmpty && str.pattern.isEmpty) None
      else Some(("String", buildConstraintKey("Str", str.minLength, str.maxLength, str.pattern)))
    case _: SchemaDefinition.Base64Bytes =>
      Some(("String", "Base64Bytes"))
    case int32: SchemaDefinition.Int32 =>
      if (int32.minimum.isEmpty && int32.maximum.isEmpty) None
      else Some(("Int", buildNumericConstraintKey("Int", int32.minimum.map(_.toLong), int32.maximum.map(_.toLong))))
    case int64: SchemaDefinition.Int64 =>
      if (int64.minimum.isEmpty && int64.maximum.isEmpty) None
      else Some(("Long", buildNumericConstraintKey("Long", int64.minimum, int64.maximum)))
    case num32: SchemaDefinition.Num32 =>
      if (num32.minimum.isEmpty && num32.maximum.isEmpty) None
      else Some(("Float", buildNumericDoubleConstraintKey("Float", num32.minimum.map(_.toDouble), num32.maximum.map(_.toDouble))))
    case num64: SchemaDefinition.Num64 =>
      if (num64.minimum.isEmpty && num64.maximum.isEmpty) None
      else Some(("Double", buildNumericDoubleConstraintKey("Double", num64.minimum, num64.maximum)))
    case SchemaDefinition.Opt(inner) =>
      constraintInfo(inner, parentSchemaName, propName)
    case _ => None
  }

  private def buildConstraintKey(
      prefix: String,
      minLength: Option[Int],
      maxLength: Option[Int],
      pattern: Option[String]
  ): String = {
    val parts = List.newBuilder[String]
    parts += prefix
    if (minLength.exists(_ > 0)) parts += "NotEmpty"
    minLength.foreach(m => parts += s"MinLen=$m")
    maxLength.foreach(m => parts += s"MaxLen=$m")
    pattern.foreach(p => parts += s"Match=$p")
    parts.result().mkString("\u0001")
  }

  private def buildNumericConstraintKey(
      prefix: String,
      minimum: Option[Long],
      maximum: Option[Long]
  ): String = {
    val parts = List.newBuilder[String]
    parts += prefix
    minimum.foreach(m => parts += s"Min=$m")
    maximum.foreach(m => parts += s"Max=$m")
    parts.result().mkString("\u0001")
  }

  private def buildNumericDoubleConstraintKey(
      prefix: String,
      minimum: Option[Double],
      maximum: Option[Double]
  ): String = {
    val parts = List.newBuilder[String]
    parts += prefix
    minimum.foreach(m => parts += s"Min=$m")
    maximum.foreach(m => parts += s"Max=$m")
    parts.result().mkString("\u0001")
  }

  /** Generate the newtype definition for Newtypes.scala */
  private def generateNewtypeDef(
      typeName: String,
      baseType: String,
      constraintKey: String
  ): Stat = {
    val constraintT = constraintKeyToIronType(constraintKey, baseType)
    val baseTypeT = Type.Name(baseType)
    
    q"""object ${Term.Name(typeName)} extends io.github.iltotore.iron.RefinedType[$baseTypeT, $constraintT]"""
  }

  /** Convert a constraint key to an Iron constraint Type using scala.meta AST construction directly. */
  private def constraintKeyToIronType(key: String, baseType: String): Type = {
    val parts = key.split("\u0001").toList.filterNot(_.isEmpty)
    val prefix = parts.headOption.getOrElse("")

    val constraints: List[Type] = parts.tail.flatMap {
      case "NotEmpty" =>
        Some(Type.Apply(Type.Name("Not"), List(Type.Name("Empty"))))
      case s if s.startsWith("MinLen=") =>
        val n = s.stripPrefix("MinLen=").toInt
        Some(Type.Apply(Type.Name("MinLength"), List(Lit.Int(n))))
      case s if s.startsWith("MaxLen=") =>
        val n = s.stripPrefix("MaxLen=").toInt
        Some(Type.Apply(Type.Name("MaxLength"), List(Lit.Int(n))))
      case s if s.startsWith("Min=") =>
        val value = s.stripPrefix("Min=")
        baseType match {
          case "Int" =>
            Some(Type.Apply(Type.Name("GreaterEqual"), List(Lit.Int(value.toDouble.toInt))))
          case "Long" =>
            Some(Type.Apply(Type.Name("GreaterEqual"), List(Lit.Long(value.toLong))))
          case "Float" | "Double" =>
            Some(Type.Apply(Type.Name("GreaterEqual"), List(Lit.Double(value.toDouble))))
          case _ => None
        }
      case s if s.startsWith("Max=") =>
        val value = s.stripPrefix("Max=")
        baseType match {
          case "Int" =>
            Some(Type.Apply(Type.Name("LessEqual"), List(Lit.Int(value.toDouble.toInt))))
          case "Long" =>
            Some(Type.Apply(Type.Name("LessEqual"), List(Lit.Long(value.toLong))))
          case "Float" | "Double" =>
            Some(Type.Apply(Type.Name("LessEqual"), List(Lit.Double(value.toDouble))))
          case _ => None
        }
      case s if s.startsWith("Match=") =>
        val regex = s.stripPrefix("Match=")
        Some(Type.Apply(Type.Name("Match"), List(Lit.String(regex))))
      case _ => None
    }

    // Add format-type constraints based on prefix
    val formatConstraints: List[Type] = prefix match {
      case "Email" =>
        List(Type.Apply(Type.Name("Match"), List(Lit.String("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$"))))
      case "Password" =>
        // Password uses Str constraints (NotEmpty/MinLen/MaxLen/Match) — already handled above
        List.empty
      case "Base64Bytes" =>
        List(Type.Apply(Type.Name("Match"), List(Lit.String("^[A-Za-z0-9+/]*=*$"))))
      case _ => List.empty
    }

    val allConstraints = formatConstraints ++ constraints

    allConstraints.reduceLeft[Type] { (a, b) =>
      Type.ApplyInfix(a, Type.Name("&"), b)
    }
  }
}
