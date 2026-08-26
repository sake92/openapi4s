package ba.sake.openapi4s

import scala.jdk.CollectionConverters._
import io.swagger.v3.oas.models.media._
import ba.sake.openapi4s.exceptions.UnsupportedSchemaDefinitionException

// https://swagger.io/docs/specification/v3_0/data-models/data-types
// https://github.com/swagger-api/swagger-core/tree/v2.2.27/modules/swagger-models/src/main/java/io/swagger/v3/oas/models/media
class SchemaDefinitionResolver {

  // named schemas that don't get their own generated model:
  // $refs to these names resolve to the mapped SchemaDefinition instead
  private var aliasSchemaDefs: Map[String, SchemaDefinition] = Map.empty

  def resolveNamedSchemas(
      schemas: Map[String, Schema[?]]
  ): NamedSchemaDefinitions = {
    aliasSchemaDefs = Map.empty

    // Phase A: resolve every named schema to a SchemaDefinition (memoized, cycle-guarded).
    val namedDefsMemo = scala.collection.mutable.Map.empty[String, SchemaDefinition]
    val inProgress = scala.collection.mutable.Set.empty[String]
    def resolveNamed(name: String): SchemaDefinition =
      namedDefsMemo.getOrElseUpdate(
        name, {
          if (inProgress.contains(name)) {
            println(s"Circular named schema reference at '${name}'. Returning Unknown schema.")
            SchemaDefinition.Unknown()
          } else {
            schemas.get(name) match {
              case None => SchemaDefinition.Ref(name)
              case Some(rawSchema) =>
                inProgress += name
                try resolveSchema(rawSchema, name)
                finally inProgress -= name
            }
          }
        }
      )

    val namedDefs = schemas.keysIterator.map(k => k -> resolveNamed(k)).toMap

    // Phase B: classify named schemas into generatable models vs aliases.
    // All nameable kinds are models; everything else (plain aliases, Opt wrappers,
    // named arrays, Unknown, Ref chains) is an alias: $refs to it resolve to the
    // underlying SchemaDefinition instead of a dangling type name.
    def isModel(def0: SchemaDefinition): Boolean = def0 match {
      case _: SchemaDefinition.Obj          => true
      case _: SchemaDefinition.Enum         => true
      case _: SchemaDefinition.EnumLiterals => true
      case _: SchemaDefinition.Const        => true
      case _: SchemaDefinition.MapObj       => true
      case _: SchemaDefinition.OneOf        => true
      case _: SchemaDefinition.AnyOf        => true
      case _: SchemaDefinition.AllOf        => true
      case _                                => false
    }

    def resolveAliasChain(name: String, seen: Set[String] = Set.empty): SchemaDefinition =
      namedDefs.get(name) match {
        case Some(SchemaDefinition.Ref(r)) if schemas.contains(r) && !seen.contains(r) =>
          resolveAliasChain(r, seen + name)
        case Some(SchemaDefinition.Ref(r)) if seen.contains(r) =>
          println(s"Circular alias reference at '${name}'. Returning Unknown schema.")
          SchemaDefinition.Unknown()
        case Some(SchemaDefinition.Ref(r)) => SchemaDefinition.Ref(r)
        case Some(other)                   => other
        case None                          => SchemaDefinition.Ref(name)
      }

    aliasSchemaDefs = namedDefs.keysIterator.flatMap { name =>
      val def0 = namedDefs(name)
      if (isModel(def0)) None
      else Some(name -> resolveAliasChain(name))
    }.toMap

    // Phase C: emit Named defs for generatable models, with alias refs normalized.
    val schemaDefs = namedDefs.toList.flatMap { case (schemaKey, schemaDef) =>
      if (isModel(schemaDef)) {
        normalize(schemaDef) match {
          case obj: SchemaDefinition.Obj          => Some(SchemaDefinition.Named(schemaKey, obj))
          case enm: SchemaDefinition.Enum         => Some(SchemaDefinition.Named(schemaKey, enm))
          case enmL: SchemaDefinition.EnumLiterals => Some(SchemaDefinition.Named(schemaKey, enmL))
          case const: SchemaDefinition.Const      => Some(SchemaDefinition.Named(schemaKey, const))
          case mapObj: SchemaDefinition.MapObj    => Some(SchemaDefinition.Named(schemaKey, mapObj))
          case oneOf: SchemaDefinition.OneOf      => Some(SchemaDefinition.Named(schemaKey, oneOf))
          case anyOf: SchemaDefinition.AnyOf      => Some(SchemaDefinition.Named(schemaKey, anyOf))
          case allOf: SchemaDefinition.AllOf      => Some(SchemaDefinition.Named(schemaKey, allOf))
          case other =>
            println(
              s"Unsupported named schema at ${schemaKey} [${other}]. Skipping the model. This may cause cascading failures!!!"
            )
            None
        }
      } else {
        println(
          s"Unsupported named schema at ${schemaKey} [${schemaDef}]. Skipping the model. This may cause cascading failures!!!"
        )
        None
      }
    }
    NamedSchemaDefinitions(schemaDefs)
  }

  /** Replaces Ref(name) with the aliased SchemaDefinition when 'name' is a non-generatable named schema. */
  def normalize(schemaDef: SchemaDefinition): SchemaDefinition =
    normalize(schemaDef, Set.empty)

  private def normalize(schemaDef: SchemaDefinition, seenAliases: Set[String]): SchemaDefinition = schemaDef match {
    case SchemaDefinition.Opt(inner) =>
      SchemaDefinition.Opt(normalize(inner, seenAliases))
    case SchemaDefinition.Arr(inner, minItems, maxItems, uniqueItems) =>
      SchemaDefinition.Arr(normalize(inner, seenAliases), minItems, maxItems, uniqueItems)
    case SchemaDefinition.OneOf(schemas, discriminatorPropertyName) =>
      SchemaDefinition.OneOf(schemas.map(normalize(_, seenAliases)), discriminatorPropertyName)
    case SchemaDefinition.AnyOf(schemas) =>
      SchemaDefinition.AnyOf(schemas.map(normalize(_, seenAliases)))
    case SchemaDefinition.AllOf(schemas) =>
      SchemaDefinition.AllOf(schemas.map(normalize(_, seenAliases)))
    case SchemaDefinition.MapObj(valueSchema) =>
      SchemaDefinition.MapObj(valueSchema.map(normalize(_, seenAliases)))
    case SchemaDefinition.Obj(properties) =>
      SchemaDefinition.Obj(properties.map(p => p.copy(schema = normalize(p.schema, seenAliases))))
    case SchemaDefinition.Ref(name) =>
      aliasSchemaDefs.get(name) match {
        case Some(_) if seenAliases.contains(name) =>
          println(s"Circular alias reference at '${name}'. Returning Unknown schema.")
          SchemaDefinition.Unknown()
        case Some(aliasDef) =>
          normalize(aliasDef, seenAliases + name)
        case None =>
          schemaDef
      }
    case other => other
  }

  def resolveSchema(schema: Schema[?], context: String): SchemaDefinition = {
    Option(schema) match {
      case None =>
        println(s"Null schema at ${context}. Returning Unknown schema.")
        SchemaDefinition.Unknown()
      case Some(schema) =>
        val defaultValue = Option(schema.getDefault).map(_.toString)
        // 'const' (OpenAPI 3.1) wins over everything else
        Option(schema.getConst.asInstanceOf[Object])
          .map(v => SchemaDefinition.Const(toEnumLiteral(v), defaultValue))
          .getOrElse {
            resolveSchemaInner(schema, context, defaultValue)
          }
    }
  }

  private def resolveSchemaInner(schema: Schema[?], context: String, defaultValue: Option[String]): SchemaDefinition = {
    val baseSchemaDef: SchemaDefinition = schema match {
      case _: StringSchema | _: PasswordSchema | _: EmailSchema | _: ByteArraySchema | _: UUIDSchema | _: DateSchema |
          _: DateTimeSchema =>
        getStringSchema(schema)
      case _: IntegerSchema =>
        getIntegerSchema(schema)
      case _: NumberSchema =>
        getNumberSchema(schema)
      case _: BooleanSchema =>
        getEnumLiterals(schema, defaultValue).getOrElse(SchemaDefinition.Bool(defaultValue))
      case _: ArraySchema =>
        getArraySchema(schema, context)
      case _: ObjectSchema | _: MapSchema =>
        getObjectSchema(schema, context)
      case _: ComposedSchema =>
        getComposedSchema(schema, context).getOrElse {
          println(s"Unsupported composed schema at ${context}. Returning Unknown schema.")
          SchemaDefinition.Unknown()
        }
      // OpenApi 3.1 complicates this a bit
      case jsonSchema: JsonSchema =>
        val types = Option(jsonSchema.getTypes).map(_.asScala).getOrElse(Set.empty)
        // we only handle the first type...
        types.headOption match {
          case Some(schemaType) =>
            schemaType match {
              case "string" =>
                getStringSchema(schema)
              case "integer" =>
                getIntegerSchema(schema)
              case "number" =>
                getNumberSchema(schema)
              case "boolean" =>
                SchemaDefinition.Bool(defaultValue)
              case "object" =>
                getObjectSchema(schema, context)
              case "array" =>
                getArraySchema(schema, context)
              case "null" =>
                throw new UnsupportedSchemaDefinitionException(s"Null is unsupported [${context}]")
            }
          case None =>
            getComposedSchema(schema, context)
              .orElse {
                Option(schema.get$ref)
                  .map { refName =>
                    val tpeName = refName.split("/").last
                    aliasSchemaDefs.get(tpeName).getOrElse(SchemaDefinition.Ref(tpeName))
                  }
              }
              .getOrElse {
                println(s"Unknown type at ${context}")
                SchemaDefinition.Unknown()
              }
        }
      case _ =>
        Option(schema.get$ref)
          .map { refName =>
            val tpeName = refName.split("/").last
            aliasSchemaDefs.get(tpeName).getOrElse(SchemaDefinition.Ref(tpeName))
          }
          .getOrElse {
            println(s"Unknown type at ${context} (${schema.getClass})")
            SchemaDefinition.Unknown()
          }

      // TODO  file, with multipart forms!...
      // case _: BinarySchema =>
    }
    val nullable = schema.getNullable
    if (nullable) SchemaDefinition.Opt(baseSchemaDef)
    else baseSchemaDef
  }

  private def getStringSchema(schema: Schema[?]): SchemaDefinition = {
    val defaultValue = Option(schema.getDefault).map(_.toString)
    val format = Option(schema.getFormat)
    Option(schema.getEnum) match {
      case Some(enumSchema) =>
        val values = enumSchema.asScala.toList.map(_.toString)
        SchemaDefinition.Enum(values, defaultValue)
      case None =>
        val minLength = Option(schema.getMinLength).map(_.intValue)
        val maxLength = Option(schema.getMaxLength).map(_.intValue)
        val pattern = Option(schema.getPattern)
        format.getOrElse("") match {
          case "password" =>
            SchemaDefinition.Password(defaultValue, minLength = minLength, maxLength = maxLength, pattern = pattern)
          case "email" =>
            SchemaDefinition.Email(defaultValue, minLength = minLength, maxLength = maxLength)
          case "byte" =>
            SchemaDefinition.Base64Bytes(defaultValue)
          case "uuid" =>
            SchemaDefinition.Uuid(defaultValue)
          case "date" =>
            SchemaDefinition.Date(defaultValue)
          case "date-time" =>
            SchemaDefinition.DateTime(defaultValue)
          case _ =>
            SchemaDefinition.Str(defaultValue, minLength = minLength, maxLength = maxLength, pattern = pattern)
        }
    }
  }

  private def getIntegerSchema(schema: Schema[?]): SchemaDefinition = {
    val defaultValue = Option(schema.getDefault).map(_.toString)
    getEnumLiterals(schema, defaultValue).getOrElse {
      val min = Option(schema.getMinimum)
      val max = Option(schema.getMaximum)
      if (schema.getFormat == "int32")
        SchemaDefinition.Int32(defaultValue, minimum = min.map(_.intValue), maximum = max.map(_.intValue))
      else
        SchemaDefinition.Int64(defaultValue, minimum = min.map(_.longValue), maximum = max.map(_.longValue))
    }
  }

  private def getNumberSchema(schema: Schema[?]): SchemaDefinition = {
    val defaultValue = Option(schema.getDefault).map(_.toString)
    getEnumLiterals(schema, defaultValue).getOrElse {
      val min = Option(schema.getMinimum)
      val max = Option(schema.getMaximum)
      if (schema.getFormat == "float")
        SchemaDefinition.Num32(defaultValue, minimum = min.map(_.floatValue), maximum = max.map(_.floatValue))
      else SchemaDefinition.Num64(defaultValue, minimum = min.map(_.doubleValue), maximum = max.map(_.doubleValue))
    }
  }

  private def getObjectSchema(schema: Schema[?], context: String): SchemaDefinition = {
    Option(schema.getAdditionalProperties) match {
      case Some(valueSchema: Schema[?]) =>
        SchemaDefinition.MapObj(Some(resolveSchema(valueSchema, context)))
      case Some(_: java.lang.Boolean) if Option(schema.getProperties).forall(_.isEmpty) =>
        SchemaDefinition.MapObj(None)
      case _ =>
        getObjectProperties(schema, context)
    }
  }

  private def getObjectProperties(schema: Schema[?], context: String): SchemaDefinition.Obj = {
    val requiredProperties = Option(schema.getRequired).map(_.asScala.toSet).getOrElse(Set.empty)
    val properties = Option(schema.getProperties)
      .map(_.asScala)
      .getOrElse(List.empty)
      .map { case (propertyKey, property) =>
        val coreSchema = resolveSchema(property, s"${context}.${propertyKey}")
        val schema = if (requiredProperties(propertyKey)) coreSchema else SchemaDefinition.Opt(coreSchema)
        SchemaProperty(propertyKey, schema)
      }
      .toList
      .distinct
    SchemaDefinition.Obj(properties)
  }

  private def getComposedSchema(schema: Schema[?], context: String): Option[SchemaDefinition] = {
    Option(schema.getOneOf())
      .map(_ => getOneOfSchema(schema, context))
      .orElse(Option(schema.getAllOf()).map(_ => getAllOfSchema(schema, context)))
      .orElse(Option(schema.getAnyOf()).map(_ => getAnyOfSchema(schema, context)))
  }

  private def getOneOfSchema(schema: Schema[?], context: String): SchemaDefinition.OneOf = {
    val schemas =
      Option(schema.getOneOf).map(_.asScala).getOrElse(List.empty).map(s => resolveSchema(s, context)).toList
    val discriminatorPropertyName = Option(schema.getDiscriminator).map(_.getPropertyName)
    SchemaDefinition.OneOf(schemas, discriminatorPropertyName = discriminatorPropertyName)
  }

  private def getAnyOfSchema(schema: Schema[?], context: String): SchemaDefinition.AnyOf = {
    val schemas =
      Option(schema.getAnyOf).map(_.asScala).getOrElse(List.empty).map(s => resolveSchema(s, context)).toList
    SchemaDefinition.AnyOf(schemas)
  }

  private def getAllOfSchema(schema: Schema[?], context: String): SchemaDefinition.AllOf = {
    val schemas =
      Option(schema.getAllOf).map(_.asScala).getOrElse(List.empty).map(s => resolveSchema(s, context)).toList
    SchemaDefinition.AllOf(schemas)
  }

  private def getArraySchema(schema: Schema[?], context: String): SchemaDefinition.Arr = {
    val arrayItemsSchema = schema.getItems
    val arrayItemsType = resolveSchema(arrayItemsSchema, context)
    val uniqueItems = Option(schema.getUniqueItems).exists(_.booleanValue)
    val minItems = Option(schema.getMinItems).map(_.intValue)
    val maxItems = Option(schema.getMaxItems).map(_.intValue)
    SchemaDefinition.Arr(arrayItemsType, minItems = minItems, maxItems = maxItems, uniqueItems = uniqueItems)
  }

  private def getEnumLiterals(schema: Schema[?], defaultValue: Option[String]): Option[SchemaDefinition.EnumLiterals] =
    Option(schema.getEnum).map { enumSchema =>
      SchemaDefinition.EnumLiterals(
        enumSchema.asScala.toList.map(v => toEnumLiteral(v.asInstanceOf[Object])),
        defaultValue
      )
    }

  private def toEnumLiteral(value: Any): EnumLiteral = value match {
    case s: String                => EnumLiteral.StrValue(s)
    case i: java.lang.Integer     => EnumLiteral.IntValue(i.intValue)
    case l: java.lang.Long        => EnumLiteral.LongValue(l.longValue)
    case b: java.lang.Boolean     => EnumLiteral.BoolValue(b.booleanValue)
    case d: java.lang.Double      => EnumLiteral.NumValue(d.doubleValue)
    case f: java.lang.Float       => EnumLiteral.NumValue(f.doubleValue)
    case bd: java.math.BigDecimal => EnumLiteral.NumValue(bd.doubleValue)
    case other =>
      println(s"Unsupported enum/const literal value: '${other}' (${other.getClass}). Treating it as a string literal.")
      EnumLiteral.StrValue(other.toString)
  }

}
