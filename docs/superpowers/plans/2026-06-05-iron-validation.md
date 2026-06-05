# Iron Validation for Circe Models — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add optional `--validation iron` flag that generates `RefinedType` newtypes from OpenAPI constraints with API-wide deduplication.

**Architecture:** New `ValidationBackend` trait orthogonal to `ModelBackend`/`FrameworkBackend`. `IronValidationBackend` walks all schemas, does 2-pass dedup naming, emits `Newtypes.scala`. `CirceModelGenerator` swaps plain types for newtypes when Iron mode is active.

**Tech Stack:** Scala 2.13, scala.meta (codegen), Iron 3.3.1 (generated code), circe (generated code)

---

## File Map

### Create
| # | File | Purpose |
|---|------|---------|
| C1 | `openapi4s/src/main/scala/ba/sake/openapi4s/ValidationBackend.scala` | Trait + ValidationBackendId sealed hierarchy |
| C2 | `openapi4s/src/main/scala/ba/sake/openapi4s/validation/NoneValidationBackend.scala` | No-op default |
| C3 | `openapi4s/src/main/scala/ba/sake/openapi4s/validation/IronValidationBackend.scala` | 2-pass collect, dedup, generate Newtypes.scala + type map |
| C4 | `openapi4s/src/main/scala/ba/sake/openapi4s/validation/ValidsonValidationBackend.scala` | Thin wrapper delegating to ValidsonUtils |

### Modify
| # | File | Change |
|---|------|--------|
| M1 | `openapi4s/src/main/scala/ba/sake/openapi4s/OpenApiWriter.scala` | Add `validation` to Config, wire ValidationBackend, call its generate |
| M2 | `openapi4s/src/main/scala/ba/sake/openapi4s/ModelBackend.scala` | Pass validation info to circe generator factory |
| M3 | `openapi4s/src/main/scala/ba/sake/openapi4s/circe/CirceModelGenerator.scala` | Accept optional `newtypeMap`, swap types, add iron.circe.given import |
| M4 | `cli/src/main/scala/ba/sake/openapi4s/cli/OpenApi4sMain.scala` | Add `--validation` arg |
| M5 | `openapi4s/src/test/scala/ba/sake/openapi4s/OpenApiGeneratorSuite.scala` | Add integration test for `--models circe --validation iron` |

---

### Task 1: Create ValidationBackend trait and ValidationBackendId

**Files:**
- Create: `openapi4s/src/main/scala/ba/sake/openapi4s/ValidationBackend.scala`

- [ ] **Step 1: Write the file**

```scala
package ba.sake.openapi4s

import ba.sake.openapi4s.OpenApiWriter.Config
import ba.sake.regenesca.GeneratedFileSource

sealed trait ValidationBackendId
object ValidationBackendId {
  case object None extends ValidationBackendId
  case object Iron extends ValidationBackendId
  case object Validson extends ValidationBackendId

  val all: List[ValidationBackendId] = List(None, Iron, Validson)

  def fromString(s: String): ValidationBackendId = s.toLowerCase match {
    case "none"     => None
    case "iron"     => Iron
    case "validson" => Validson
    case _ =>
      throw new RuntimeException(
        s"Unknown validation backend '${s}'. Available validation backends: 'none', 'iron', 'validson'"
      )
  }
}

trait ValidationBackend {
  def id: ValidationBackendId
  def supportedModelIds: Set[ModelBackendId]
  def generate(
      config: Config,
      openApiDefinition: OpenApiDefinition
  ): (Seq[GeneratedFileSource], Map[String, Map[String, String]])
}

object ValidationBackend {

  val none: ValidationBackend = ???
  val iron: ValidationBackend = ???
  val validson: ValidationBackend = ???

  val byId: Map[ValidationBackendId, ValidationBackend] = Map(
    ValidationBackendId.None -> none,
    ValidationBackendId.Iron -> iron,
    ValidationBackendId.Validson -> validson
  )
}
```

Note: The `generate` return type includes a `Map[String, Map[String, String]]` which is `schemaName -> (propName -> typeName)`. The `CirceModelGenerator` uses this to swap plain types for newtypes. The `iron.circe.given` import handles serialization.

- [ ] **Step 2: Run compile to verify**

```bash
./mill openapi4s.compile
```

Expected: compilation error (references to validson/none/iron backends not yet created). This is fine — we'll create them in next tasks.

- [ ] **Step 3: Commit**

```bash
git add openapi4s/src/main/scala/ba/sake/openapi4s/ValidationBackend.scala
git commit -m "feat: add ValidationBackend trait and ValidationBackendId"
```

---

### Task 2: Create NoneValidationBackend

**Files:**
- Create: `openapi4s/src/main/scala/ba/sake/openapi4s/validation/NoneValidationBackend.scala`

- [ ] **Step 1: Write the file**

```scala
package ba.sake.openapi4s
package validation

import ba.sake.openapi4s.OpenApiWriter.Config
import ba.sake.regenesca.GeneratedFileSource

object NoneValidationBackend extends ValidationBackend {
  override val id: ValidationBackendId = ValidationBackendId.None
  override val supportedModelIds: Set[ModelBackendId] =
    Set(ModelBackendId.NoModel, ModelBackendId.Circe, ModelBackendId.Tupson)

  override def generate(
      config: Config,
      openApiDefinition: OpenApiDefinition
  ): (Seq[GeneratedFileSource], Map[String, Map[String, String]]) =
    (Seq.empty, Map.empty)
}
```

- [ ] **Step 2: Wire into ValidationBackend object**. Update `ValidationBackend.scala` — replace the `???` stubs:

In `ValidationBackend.scala`, change:
```scala
  val none: ValidationBackend = ???
  val iron: ValidationBackend = ???
  val validson: ValidationBackend = ???
```
to:
```scala
  val none: ValidationBackend = validation.NoneValidationBackend
  val iron: ValidationBackend = validation.IronValidationBackend
  val validson: ValidationBackend = validation.ValidsonValidationBackend
```

- [ ] **Step 3: Compile**

```bash
./mill openapi4s.compile
```

Expected: compile succeeds (IronValidationBackend and ValidsonValidationBackend not yet created but we'll add stubs in next task if needed, or leave them as pending compilation errors — will be resolved by Task 3 and 4).

- [ ] **Step 4: Commit**

```bash
git add openapi4s/src/main/scala/ba/sake/openapi4s/validation/NoneValidationBackend.scala
git add openapi4s/src/main/scala/ba/sake/openapi4s/ValidationBackend.scala
git commit -m "feat: add NoneValidationBackend"
```

---

### Task 3: Create IronValidationBackend — constraint mapping + 2-pass dedup

**Files:**
- Create: `openapi4s/src/main/scala/ba/sake/openapi4s/validation/IronValidationBackend.scala`

This is the core task. The backend:
1. Walks all named schemas collecting constrained properties
2. Groups by property name
3. 2-pass resolution: same constraint → shared name; different constraints → `SchemaNamePropName`
4. Generates `Newtypes.scala` and returns the type map for `CirceModelGenerator`

- [ ] **Step 1: Write the file**

```scala
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
    }

    // --- Pass 2: group by property name, resolve naming conflicts ---
    val byPropName: Map[String, List[PropEntry]] = allEntries.groupBy(_.propName)

    // Map: (schemaName, propName) -> typeName for CirceModelGenerator
    val typeMap = scala.collection.mutable.Map.empty[String, Map[String, String]]
    // Accumulator for Newtypes.scala statements
    val newtypeStmts = scala.collection.mutable.ListBuffer.empty[Stat]

    byPropName.foreach { case (propName, entries) =>
      val constraintKeys = entries.map(_.constraintKey).distinct

      if (constraintKeys.size == 1) {
        // Single constraint: shared name = capitalized propName
        val typeName = propName.capitalize
        val entry = entries.head
        emitNewtype(newtypeStmts, typeName, entry.baseType, entry.constraintKey)

        entries.foreach { e =>
          val sm = typeMap.getOrElse(e.schemaName, Map.empty)
          typeMap(e.schemaName) = sm + (e.propName -> typeName)
        }
      } else {
        // Different constraints: prefix with schema name
        entries.foreach { entry =>
          val typeName = s"${entry.schemaName}${entry.propName.capitalize}"
          emitNewtype(newtypeStmts, typeName, entry.baseType, entry.constraintKey)

          val sm = typeMap.getOrElse(entry.schemaName, Map.empty)
          typeMap(entry.schemaName) = sm + (entry.propName -> typeName)
        }
      }
      // Also handle format-based: email, password, base64Bytes — always emit with fixed name
    }

    // Handle well-known format types (always emitted if any appear)
    handleFormatTypes(allEntries, newtypeStmts, typeMap)

    if (newtypeStmts.isEmpty) {
      (Seq.empty, Map.empty)
    } else {
      val modelsPkg = config.basePackage.split("\\.").toList
        .map(Term.Name(_))
        .reduceLeft[Term.Ref](Term.Select(_, _))

      val imports = List[Import](
        q"import io.github.iltotore.iron.*",
        q"import io.github.iltotore.iron.constraint.all.*"
      )

      val source = source"""
        // generated new types with OpenApi4s
        package ${modelsPkg} {
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

  /** Returns (baseType, constraintKey) for a given schema definition, if it has any constraints. */
  private def constraintInfo(
      schemaDef: SchemaDefinition,
      parentSchemaName: String,
      propName: String
  ): Option[(String, String)] = schemaDef match {
    case email: SchemaDefinition.Email =>
      Some(("String", buildConstraintKey("Email", email.minLength, email.maxLength, None)))
    case pwd: SchemaDefinition.Password =>
      Some(("String", buildConstraintKey("Password", pwd.minLength, pwd.maxLength, pwd.pattern)))
    case str: SchemaDefinition.Str =>
      if (str.minLength.isEmpty && str.maxLength.isEmpty && str.pattern.isEmpty) None
      else Some(("String", buildConstraintKey("Str", str.minLength, str.maxLength, str.pattern)))
    case base64: SchemaDefinition.Base64Bytes =>
      Some(("String", "Base64Bytes"))
    case int32: SchemaDefinition.Int32 =>
      if (int32.minimum.isEmpty && int32.maximum.isEmpty) None
      else Some(("Int", buildNumericConstraintKey("Int", int32.minimum.map(_.toLong), int32.maximum.map(_.toLong))))
    case int64: SchemaDefinition.Int64 =>
      if (int64.minimum.isEmpty && int64.maximum.isEmpty) None
      else Some(("Long", buildNumericConstraintKey("Long", int64.minimum, int64.maximum)))
    case num32: SchemaDefinition.Num32 =>
      if (num32.minimum.isEmpty && num32.maximum.isEmpty) None
      else Some(("Float", buildNumericConstraintKey("Float", num32.minimum.map(_.toDouble), num32.maximum.map(_.toDouble))))
    case num64: SchemaDefinition.Num64 =>
      if (num64.minimum.isEmpty && num64.maximum.isEmpty) None
      else Some(("Double", buildNumericConstraintKey("Double", num64.minimum, num64.maximum)))
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
    parts.result().mkString("|")
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
    parts.result().mkString("|")
  }

  private def buildNumericConstraintKey(
      prefix: String,
      minimum: Option[Double],
      maximum: Option[Double]
  ): String = {
    val parts = List.newBuilder[String]
    parts += prefix
    minimum.foreach(m => parts += s"Min=$m")
    maximum.foreach(m => parts += s"Max=$m")
    parts.result().mkString("|")
  }

  /** Emit a newtype definition into the statements buffer. */
  private def emitNewtype(
      buf: scala.collection.mutable.ListBuffer[Stat],
      typeName: String,
      baseType: String,
      constraintKey: String
  ): Unit = {
    val typeNameT = Type.Name(typeName)
    val baseTypeT = Type.Name(baseType)
    val constraintT = constraintKeyToIronType(constraintKey, baseType)

    buf += q"type ${typeNameT} = ${Term.Name(typeName)}.T"
    buf += q"object ${Term.Name(typeName)} extends io.github.iltotore.iron.RefinedType[${baseTypeT}, ${constraintT}]"
  }

  /** Convert a constraint key string to an Iron constraint Type. */
  private def constraintKeyToIronType(key: String, baseType: String): Type = {
    val parts = key.split("\\|").toList.filterNot(_.isEmpty)

    val constraints: List[Type] = parts.flatMap {
      case "NotEmpty"   => Some(t"io.github.iltotore.iron.constraint.all.Not[io.github.iltotore.iron.constraint.all.Empty]")
      case s if s.startsWith("MinLen=") =>
        val n = s.stripPrefix("MinLen=").toInt
        Some(t"io.github.iltotore.iron.constraint.all.MinLength[$n]")
      case s if s.startsWith("MaxLen=") =>
        val n = s.stripPrefix("MaxLen=").toInt
        Some(t"io.github.iltotore.iron.constraint.all.MaxLength[$n]")
      case s if s.startsWith("Min=") =>
        baseType match {
          case "Int" | "Long" =>
            val n = s.stripPrefix("Min=").toDouble.toLong
            Some(t"io.github.iltotore.iron.constraint.all.GreaterEqual[$n]")
          case "Float" | "Double" =>
            val d = s.stripPrefix("Min=").toDouble
            Some(t"io.github.iltotore.iron.constraint.all.GreaterEqual[$d]")
          case _ => None
        }
      case s if s.startsWith("Max=") =>
        baseType match {
          case "Int" | "Long" =>
            val n = s.stripPrefix("Max=").toDouble.toLong
            Some(t"io.github.iltotore.iron.constraint.all.LessEqual[$n]")
          case "Float" | "Double" =>
            val d = s.stripPrefix("Max=").toDouble
            Some(t"io.github.iltotore.iron.constraint.all.LessEqual[$d]")
          case _ => None
        }
      case s if s.startsWith("Match=") =>
        val regex = s.stripPrefix("Match=")
        Some(t"io.github.iltotore.iron.constraint.all.Match[$regex]")
      case "Email" =>
        Some(t"""io.github.iltotore.iron.constraint.all.Match["^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$$"]""")
      case "Password" | "Str" => None // handled by NotEmpty/MinLen/MaxLen/Match sub-constraints
      case "Base64Bytes" =>
        Some(t"""io.github.iltotore.iron.constraint.all.Match["^[A-Za-z0-9+/]*=*$$"]""")
      case _ => None
    }

    constraints.reduceLeftOption[Type] { (a, b) =>
      t"$a & $b"
    }.getOrElse(t"io.github.iltotore.iron.constraint.all.True")
  }

  /** Handle well-known format types (Email, Password, Base64Bytes). */
  private def handleFormatTypes(
      allEntries: List[PropEntry],
      newtypeStmts: scala.collection.mutable.ListBuffer[Stat],
      typeMap: scala.collection.mutable.Map[String, Map[String, String]]
  ): Unit = {
    val formatTypes = Map(
      "Email" -> "Email",
      "Password" -> "Password",
      "Base64Bytes" -> "Base64Bytes"
    )

    formatTypes.foreach { case (formatName, typeName) =>
      val matching = allEntries.filter(e =>
        e.propName.equalsIgnoreCase(formatName) || e.constraintKey.startsWith(formatName)
      )
      if (matching.nonEmpty && !newtypeStmts.exists {
        case q"type $tname = $_.T" => tname.toString == typeName
        case _ => false
      }) {
        val entry = matching.head
        emitNewtype(newtypeStmts, typeName, entry.baseType, entry.constraintKey)

        matching.foreach { e =>
          val sm = typeMap.getOrElse(e.schemaName, Map.empty)
          // For format types, always use the fixed name regardless of what the property is called
          typeMap(e.schemaName) = sm + (e.propName -> typeName)
        }
      }
    }
  }
}
```

> **Note for implementer:** The `constraintKeyToIronType` method uses `t"..."` interpolators that reference Iron constraint types via fully-qualified names. This is intentional — the `Newtypes.scala` file imports `io.github.iltotore.iron.constraint.all.*` so the generated code is clean. The interpolator `t"..."` with string content requires careful quoting for regex patterns containing `$`.

- [ ] **Step 2: Handle constraint-based properties in typeMap — account for Option wrapping**

After computing the `typeMap` in the main `generate` method, the map stores the *base* type name (without `Option[...]`). The `CirceModelGenerator` will handle wrapping in `Option[...]` based on the schema definition. So in the map we store just `"Email"`, and `CirceModelGenerator` uses `t"Option[Email]"` when the property is `SchemaDefinition.Opt(Email(...))`.

- [ ] **Step 3: Compile**

```bash
./mill openapi4s.compile
```

Expected: may have type errors in `constraintKeyToIronType` related to `t"..."` interpolation. Fix any issues — the key challenge is interpolating regex strings with `$` and backslashes.

- [ ] **Step 4: Commit**

```bash
git add openapi4s/src/main/scala/ba/sake/openapi4s/validation/IronValidationBackend.scala
git commit -m "feat: add IronValidationBackend with 2-pass dedup"
```

---

### Task 4: Create ValidsonValidationBackend (refactor from TupsonModelGenerator)

**Files:**
- Create: `openapi4s/src/main/scala/ba/sake/openapi4s/validation/ValidsonValidationBackend.scala`
- Modify: `openapi4s/src/main/scala/ba/sake/openapi4s/tupson/TupsonModelGenerator.scala`

- [ ] **Step 1: Write ValidsonValidationBackend**

```scala
package ba.sake.openapi4s
package validation

import ba.sake.openapi4s.OpenApiWriter.Config
import ba.sake.regenesca.GeneratedFileSource

object ValidsonValidationBackend extends ValidationBackend {
  override val id: ValidationBackendId = ValidationBackendId.Validson
  override val supportedModelIds: Set[ModelBackendId] = Set(ModelBackendId.Tupson)

  override def generate(
      config: Config,
      openApiDefinition: OpenApiDefinition
  ): (Seq[GeneratedFileSource], Map[String, Map[String, String]]) =
    // Validson is integrated directly into TupsonModelGenerator via ValidsonUtils.
    // No separate file generation needed.
    (Seq.empty, Map.empty)
}
```

- [ ] **Step 2: Compile**

```bash
./mill openapi4s.compile
```

- [ ] **Step 3: Commit**

```bash
git add openapi4s/src/main/scala/ba/sake/openapi4s/validation/ValidsonValidationBackend.scala
git commit -m "feat: add ValidsonValidationBackend"
```

---

### Task 5: Modify OpenApiWriter — Config, apply(), write()

**Files:**
- Modify: `openapi4s/src/main/scala/ba/sake/openapi4s/OpenApiWriter.scala`

- [ ] **Step 1: Add `validation` field to Config**

In `OpenApiWriter.scala`, inside `object OpenApiWriter`, change the Config case class:

Current:
```scala
  case class Config(
      url: String,
      baseFolder: Path,
      basePackage: String,
      models: String,
      framework: String
  )
```

New:
```scala
  case class Config(
      url: String,
      baseFolder: Path,
      basePackage: String,
      models: String,
      framework: String,
      validation: String = "none"
  )
```

- [ ] **Step 2: Wire ValidationBackend in apply()**

In `OpenApiWriter.apply(config: Config)`, after the model/framework validation, add:

```scala
    val validationId = ValidationBackendId.fromString(config.validation)
    val validationBackend = ValidationBackend.byId(validationId)

    if (!validationBackend.supportedModelIds.contains(modelId)) {
      throw new RuntimeException(
        s"Incompatible config: --models ${config.models} does not support --validation ${config.validation}. " +
          s"Validation '${validationId}' is only compatible with model backends: ${validationBackend.supportedModelIds.mkString(", ")}"
      )
    }
```

And change the constructor call to pass `validationBackend`:
```scala
    new OpenApiWriter(config, modelBackend, frameworkBackend, validationBackend)
```

- [ ] **Step 3: Update class constructor and write()**

Change the class definition:
```scala
class OpenApiWriter(
    config: OpenApiWriter.Config,
    modelBackend: ModelBackend,
    frameworkBackend: FrameworkBackend,
    validationBackend: ValidationBackend
) {
```

In `write()`, call the validation backend:
```scala
  def write(): Seq[GeneratedFileSource] = {
    println(
      s"Started generating OpenApi for '${config.url}' with models='${config.models}', framework='${config.framework}', validation='${config.validation}' ..."
    )
    val (validationSources, validationTypeMap) = validationBackend.generate(config, openapiDefinition)
    val modelSources = modelBackend.generator(config, openapiDefinition, validationTypeMap).generate()
    val modelContract = modelBackend.contract(config)
    val frameworkSources = frameworkBackend.generator(config, openapiDefinition, modelContract).generate()
    // ... rest unchanged
    val adaptedGenSourceFiles = (validationSources ++ modelSources ++ frameworkSources).map { ... }
    // ...
  }
```

> **Note:** This changes the `ModelBackend.generator` signature to accept `validationTypeMap`. Update in Task 6.

- [ ] **Step 4: Compile and fix compilation errors**

```bash
./mill openapi4s.compile
```

Expected errors: `ModelBackend.generator` signature mismatch, `OpenApiGenerator` needs new signature. Fix in the next tasks.

- [ ] **Step 5: Commit**

```bash
git add openapi4s/src/main/scala/ba/sake/openapi4s/OpenApiWriter.scala
git commit -m "feat: add --validation config and wire ValidationBackend"
```

---

### Task 6: Modify ModelBackend — pass validationTypeMap to generators

**Files:**
- Modify: `openapi4s/src/main/scala/ba/sake/openapi4s/ModelBackend.scala`

- [ ] **Step 1: Update trait method signature**

Change:
```scala
  def generator(config: Config, openapiDefinition: OpenApiDefinition): OpenApiGenerator
```
To:
```scala
  def generator(
      config: Config, 
      openapiDefinition: OpenApiDefinition, 
      validationTypeMap: Map[String, Map[String, String]] = Map.empty
  ): OpenApiGenerator
```

The default `Map.empty` preserves backward compat for existing callers.

- [ ] **Step 2: Update circe backend to pass validationTypeMap**

In `val circe: ModelBackend`, change:
```scala
    override def generator(config: Config, openapiDefinition: OpenApiDefinition, validationTypeMap: Map[String, Map[String, String]] = Map.empty): OpenApiGenerator =
      new CirceModelGenerator(config, openapiDefinition, validationTypeMap)
```

- [ ] **Step 3: Update none backend**

```scala
    override def generator(config: Config, openapiDefinition: OpenApiDefinition, validationTypeMap: Map[String, Map[String, String]] = Map.empty): OpenApiGenerator =
      new OpenApiGenerator { ... }
```

- [ ] **Step 4: Update tupson backend**

```scala
    override def generator(config: Config, openapiDefinition: OpenApiDefinition, validationTypeMap: Map[String, Map[String, String]] = Map.empty): OpenApiGenerator =
      new TupsonModelGenerator(config, openapiDefinition)
```

- [ ] **Step 5: Compile**

```bash
./mill openapi4s.compile
```

Expected: CirceModelGenerator needs a new constructor parameter. Fix in next task.

- [ ] **Step 6: Commit**

```bash
git add openapi4s/src/main/scala/ba/sake/openapi4s/ModelBackend.scala
git commit -m "feat: pass validationTypeMap through ModelBackend to generators"
```

---

### Task 7: Modify CirceModelGenerator — accept newtypeMap, swap types, add import

**Files:**
- Modify: `openapi4s/src/main/scala/ba/sake/openapi4s/circe/CirceModelGenerator.scala`

- [ ] **Step 1: Add constructor parameter**

Change:
```scala
class CirceModelGenerator(config: OpenApiWriter.Config, openApiDefinition: OpenApiDefinition)
    extends OpenApiGenerator {
```
To:
```scala
class CirceModelGenerator(
    config: OpenApiWriter.Config,
    openApiDefinition: OpenApiDefinition,
    validationTypeMap: Map[String, Map[String, String]] = Map.empty
) extends OpenApiGenerator {
```

- [ ] **Step 2: Add iron.circe.given import when Iron mode is active**

In `generate()` method, change the imports list. Currently:
```scala
    val modelImports = List[Import](
      q"import java.time.*",
      q"import java.util.UUID",
      q"import io.circe.{Codec, Json}",
      q"import io.circe.derivation.{Configuration, ConfiguredCodec, ConfiguredEnumCodec}"
    )
```

Change to:
```scala
    val baseImports = List[Import](
      q"import java.time.*",
      q"import java.util.UUID",
      q"import io.circe.{Codec, Json}",
      q"import io.circe.derivation.{Configuration, ConfiguredCodec, ConfiguredEnumCodec}"
    )
    val ironImport = if (validationTypeMap.nonEmpty) List[Import](
      q"import io.github.iltotore.iron.circe.given"
    ) else List.empty
    val modelImports = baseImports ++ ironImport
```

- [ ] **Step 3: Swap plain types for newtypes in property type resolution**

In `generateModelSources`, when building params for `SchemaDefinition.Obj`, change:
```scala
            val propertyTpe = SchemaUtils.resolveType(
              property.schema,
              Some(property.name),
              Some(namedSchemaName),
              allowNullable = true,
              context = s"${namedSchemaName}.${property.name}",
              fallbackAnyType = t"Json"
            )
            Some(param"${Term.Name(property.name)}: ${propertyTpe}")
```

To:
```scala
            val resolvedType = SchemaUtils.resolveType(
              property.schema,
              Some(property.name),
              Some(namedSchemaName),
              allowNullable = true,
              context = s"${namedSchemaName}.${property.name}",
              fallbackAnyType = t"Json"
            )
            val propertyTpe = validationTypeMap
              .get(namedSchemaName)
              .flatMap(_.get(property.name))
              .map { newtypeName =>
                // Check if the original schema is Optional — wrap in Option[...] if needed
                property.schema match {
                  case SchemaDefinition.Opt(_) => t"Option[${Type.Name(newtypeName)}]"
                  case _ => Type.Name(newtypeName)
                }
              }
              .getOrElse(resolvedType)
            Some(param"${Term.Name(property.name)}: ${propertyTpe}")
```

- [ ] **Step 4: Compile**

```bash
./mill openapi4s.compile
```

- [ ] **Step 5: Commit**

```bash
git add openapi4s/src/main/scala/ba/sake/openapi4s/circe/CirceModelGenerator.scala
git commit -m "feat: swap plain types for Iron newtypes in CirceModelGenerator"
```

---

### Task 8: Add --validation CLI arg

**Files:**
- Modify: `cli/src/main/scala/ba/sake/openapi4s/cli/OpenApi4sMain.scala`

- [ ] **Step 1: Add --validation argument**

After the `basePackage` arg, add:

```scala
      @arg(doc = "Validation backend: 'none', 'iron' or 'validson'. If unset, defaults to 'none'.")
      validation: String = "none"
```

- [ ] **Step 2: Pass validation to Config**

In the `run` method, add `validation = validation` to the Config creation:
```scala
    val writer = OpenApiWriter(
      config = OpenApiWriter.Config(
        url = url,
        baseFolder = Paths.get(baseFolder),
        basePackage = basePackage,
        models = models,
        framework = framework,
        validation = validation
      )
    )
```

- [ ] **Step 3: Compile CLI**

```bash
./mill cli.compile
```

- [ ] **Step 4: Commit**

```bash
git add cli/src/main/scala/ba/sake/openapi4s/cli/OpenApi4sMain.scala
git commit -m "feat: add --validation CLI argument"
```

---

### Task 9: Integration test

**Files:**
- Modify: `openapi4s/src/test/scala/ba/sake/openapi4s/OpenApiGeneratorSuite.scala`

- [ ] **Step 1: Add test for circe + iron validation**

Add a new test method to the suite:

```scala
  test("composed generator should support circe + iron validation") {
    val baseFolder = Files.createTempDirectory("openapi4s-circe-iron")
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl("petstore_3.0.0.json"),
      baseFolder = baseFolder,
      basePackage = "pkg",
      models = "circe",
      framework = "none",
      validation = "iron"
    )
    OpenApiWriter(config).write()
    val generatedFiles = listScalaFiles(baseFolder.resolve("pkg"))
    assert(generatedFiles.nonEmpty)
    assert(generatedFiles.exists(_.startsWith("models/Newtypes.scala")))
    assert(generatedFiles.exists(_.startsWith("models/")))
    
    val newtypesFile = readGeneratedFile(baseFolder.resolve("pkg"), "models/Newtypes.scala")
    // Check for well-known types from petstore (name field with constraints)
    assert(newtypesFile.contains("import io.github.iltotore.iron.*"))
    assert(newtypesFile.contains("import io.github.iltotore.iron.constraint.all.*"))
    
    val petFile = readGeneratedFile(baseFolder.resolve("pkg"), "models/Pet.scala")
    assert(petFile.contains("import io.github.iltotore.iron.circe.given"))
  }
```

- [ ] **Step 2: Run the test**

```bash
./mill openapi4s.test
```

Expected: test passes, showing Newtypes.scala generated with type definitions.

- [ ] **Step 3: Also verify backward compat — existing tests still pass**

Already verified by running the full test suite. The new code adds the `validation` field with default `"none"`, so existing tests should be unaffected.

- [ ] **Step 4: Commit**

```bash
git add openapi4s/src/test/scala/ba/sake/openapi4s/OpenApiGeneratorSuite.scala
git commit -m "test: add integration test for circe + iron validation"
```

---

### Task 10: Run full test suite and verify

- [ ] **Step 1: Run all tests**

```bash
./mill __.test
```

Expected: all tests pass (existing 15 + 1 new). No regressions.

- [ ] **Step 2: Run scalafmt**

```bash
./mill __.reformat
```

- [ ] **Step 3: Final commit if any formatting changes**

```bash
git add -u && git commit -m "chore: scalafmt formatting"
```
