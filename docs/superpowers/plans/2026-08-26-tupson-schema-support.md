# Tupson Schema Coverage Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend openapi4s so the tupson backend generates named tuples for anonymous objects, union types for ad hoc `oneOf`/`anyOf`, literal (union) types for `const`/enums, and maps for `additionalProperties` — with circe/framework output unchanged except native map support.

**Architecture:** New `TupsonTypeResolver` (in the `tupson` package) resolves tupson model types and delegates unchanged leaf cases (scalars, Ref, Named, Unknown) to the existing `SchemaUtils.resolveType`. `SchemaUtils.resolveType` itself gains cases only for the new ADT nodes, mapping them to today's equivalent output (so circe/frameworks are behavior-identical). `SchemaDefinition` gains `Const`, `EnumLiterals`, `MapObj`, `AnyOf`; `OneOf.discriminatorPropertyName` becomes `Option[String]`.

**Tech Stack:** Scala 2.13 (mill) generator via scalameta 4.9.9 + Scala34 dialect (already sufficient — verified), swagger-parser 2.1.24, generated Scala 3 code targets tupson ≥ 0.20.0 / Scala 3.7+.

**Verified facts this plan relies on:** scalameta 4.9.9 emits named tuples / unions / literal types with the existing `import scala.meta.dialects.Scala34`; tupson 0.20.0 (on Maven Central) roundtrips named tuples (incl. nested in `Option`/`Seq`, inline literal-union fields, 30-field arity), unions, and literals on Scala 3.7.1; `Schema.getConst()/getAnyOf()/getAdditionalProperties()` exist in swagger-models; `additionalProperties` has no default-`true` trap (null unless set); circe has no named-tuple/union/literal support (hence unchanged there).

---

### Task 0: Test dependencies

**Files:** Modify `build.mill:23-28`

- [ ] **Step 1: Update test deps** — replace `mvn"org.scala-lang:scala3-compiler_3:3.3.6"` with `3.7.3`, add tupson:

```scala
mvn"org.scala-lang:scala3-compiler_3:3.7.3",
mvn"ba.sake:tupson_3:0.20.0",
```

(tupson brings jawn-ast transitively — needed for the generated `JValue` import.)

- [ ] **Step 2: Verify existing suites still pass** — Run: `./mill openapi4s.test` — Expected: all green (Scala 3.7 compiler is source-compatible for the existing generated 3.3-style code).
- [ ] **Step 3: Commit** — `git add build.mill && git commit -m "test: bump scala3-compiler to 3.7.3, add tupson 0.20.0 test dep"`

---

### Task 1: `SchemaDefinition` ADT

**Files:** Modify `openapi4s/src/main/scala/ba/sake/openapi4s/SchemaDefinition.scala`

- [ ] **Step 1: Add `EnumLiteral`** (top level, before `SchemaProperty`):

```scala
sealed abstract class EnumLiteral
object EnumLiteral {
  case class StrValue(value: String) extends EnumLiteral
  case class IntValue(value: Int) extends EnumLiteral
  case class LongValue(value: Long) extends EnumLiteral
  case class NumValue(value: Double) extends EnumLiteral
  case class BoolValue(value: Boolean) extends EnumLiteral
}
```

- [ ] **Step 2: Add new cases + change `OneOf`** in `object SchemaDefinition`:

```scala
case class Const(value: EnumLiteral, default: Option[String]) extends NameableSchemaDefinition
case class EnumLiterals(values: List[EnumLiteral], default: Option[String]) extends NameableSchemaDefinition
case class MapObj(valueSchema: Option[SchemaDefinition]) extends NameableSchemaDefinition
case class AnyOf(schemas: List[SchemaDefinition]) extends NameableSchemaDefinition
// changed:
case class OneOf(schemas: List[SchemaDefinition], discriminatorPropertyName: Option[String])
    extends NameableSchemaDefinition
```

- [ ] **Step 3: Fix compile errors mechanically**
  - `SchemaDefinitionResolver.scala:181-185` — `getOneOfSchema` → wrap in `Option(...)` (real logic in Task 2).
  - `circe/CirceModelGenerator.scala:168-170` — `Lit.String(oneOfSchema.discriminatorPropertyName.getOrElse("@type"))`.
  - `tupson/TupsonModelGenerator.scala:129` — `Lit.String(oneOfSchema.discriminatorPropertyName.getOrElse("@type"))` (temporary; rewritten in Task 5).
  - Add `case _ => List.empty` catch-alls to both generators' `namedSchemaDef.schema match` (circe `:149` area, tupson `:48` area) with `println(s"Unsupported named schema type: ...")` — prevents `MatchError` on the new ADT cases.
- [ ] **Step 4: Run** `./mill openapi4s.compile` — Expected: PASS.
- [ ] **Step 5: Commit** — `git commit -m "feat: extend SchemaDefinition ADT (Const, EnumLiterals, MapObj, AnyOf, optional OneOf discriminator)"`

---

### Task 2: `SchemaDefinitionResolver` parsing

**Files:** Modify `openapi4s/src/main/scala/ba/sake/openapi4s/SchemaDefinitionResolver.scala`; Test: `openapi4s/src/test/scala/ba/sake/openapi4s/SchemaDefinitionResolverSuite.scala`

- [ ] **Step 1: Write failing tests** (append to `SchemaDefinitionResolverSuite`, following the existing `petstore` assertion style):

```scala
test("resolve tupson_features.yaml named schemas") { /* typed ADT assertions, see Step 4 resources */ }
```

Assert: anonymous `Obj` stays `Obj`; inline enum → `Enum(List("a","b","c"), None)`; `OneOf(schemas, None)` without discriminator; `OneOf(schemas, Some("pet_type"))` with; `AnyOf(...)`; integer enum → `EnumLiterals(List(IntValue(1), IntValue(2)), None)`; boolean enum → `EnumLiterals(List(BoolValue(true), BoolValue(false)), None)`; `additionalProperties: {type: string}` → `MapObj(Some(Str(...)))`; `additionalProperties: true` + no properties → `MapObj(None)`; const (3.1 spec) → `Const(StrValue("abc"), None)`.

- [ ] **Step 2: Run** `./mill openapi4s.test ba.sake.openapi4s.SchemaDefinitionResolverSuite` — Expected: FAIL.
- [ ] **Step 3: Implement in resolver**
  - `resolveSchema` — const check first, before the type match:

```scala
Option(schema.getConst).map(toEnumLiteral).map(l => SchemaDefinition.Const(l, defaultValue)) match {
  case Some(const) => return const
  case None        => ()
}
```

  - `toEnumLiteral(Object)` helper → `StrValue`/`IntValue` (Integer)/`LongValue` (Long)/`NumValue` (Double/Float/BigDecimal)/`BoolValue`; unknown → `println` warning + treat as `StrValue(v.toString)`.
  - `getStringSchema` — unchanged (string enums stay `Enum(List[String])`).
  - `getIntegerSchema`/`getNumberSchema`/Boolean branch — enum check first: `Option(schema.getEnum).map(...)` → `EnumLiterals(...)`; else existing logic.
  - `getComposedSchema` — add `.orElse(Option(schema.getAnyOf()).map(_ => getAnyOfSchema(schema, context)))` after oneOf/allOf; `getAnyOfSchema` mirrors `getOneOfSchema` returning `AnyOf`.
  - `getOneOfSchema` — `SchemaDefinition.OneOf(schemas, Option(schema.getDiscriminator).map(_.getPropertyName))` (delete the `"@type"` default + println).
  - `getObjectSchema` — additionalProperties branch:

```scala
Option(schema.getAdditionalProperties) match {
  case Some(s: Schema[?]) => SchemaDefinition.MapObj(Some(resolveSchema(s, context)))
  case Some(_: java.lang.Boolean) if Option(schema.getProperties).forall(_.isEmpty) => SchemaDefinition.MapObj(None)
  case _ => /* existing properties logic */
}
```

  - `resolveNamedSchemas` — add `Const`, `EnumLiterals`, `MapObj`, `AnyOf` to the named matches.
- [ ] **Step 4: Add test resources** `openapi4s/src/test/resources/tupson_features.yaml` (3.0: anonymous object inline/nested/in-array, inline multi-value enum, single-value enum, integer enum, boolean enum, `additionalProperties` map + free-form, `anyOf`, oneOf with & without discriminator) and `tupson_features_31.yaml` (3.1: `const`). Keep existing `oneOf_anonymous.yaml` as-is.
- [ ] **Step 5: Run** suite — Expected: PASS.
- [ ] **Step 6: Commit** — `git commit -m "feat: resolve const, anyOf, typed enums, additionalProperties, optional oneOf discriminator"`

---

### Task 3: Shared `SchemaUtils.resolveType` new cases

**Files:** Modify `openapi4s/src/main/scala/ba/sake/openapi4s/SchemaUtils.scala`

- [ ] **Step 1: Add cases** (preserve circe/framework behavior exactly — these constructs previously never reached the shared resolver, except `Unknown` fallbacks):

```scala
case _: SchemaDefinition.Const         => fallbackAnyType                    // today: Unknown -> Json/JValue
case _: SchemaDefinition.AnyOf         => fallbackAnyType                    // today: dropped to Unknown
case el: SchemaDefinition.EnumLiterals => enumLiteralsPlainType(el)           // today: plain Int/Long/Double/Boolean/String
case SchemaDefinition.MapObj(valueSchemaOpt) =>
  valueSchemaOpt match {
    case Some(vs) => t"Map[String, ${resolveType(vs, propertyName, parentTypeName, allowNullable = allowNullable, context, fallbackAnyType)}]"
    case None     => t"Map[String, ${fallbackAnyType}]"
  }
```

with `private def enumLiteralsPlainType(el: EnumLiterals): Type = el.values.headOption.map { case _: IntValue => t"Int"; case _: LongValue => t"Long"; case _: NumValue => t"Double"; case _: BoolValue => t"Boolean"; case _: StrValue => t"String" }.getOrElse(fallbackAnyType)`.

- [ ] **Step 2: Run** `./mill openapi4s.test` — Expected: existing suites green (nothing observable changes for circe/frameworks).
- [ ] **Step 3: Commit** — `git commit -m "feat: handle new schema kinds in shared resolver with legacy-compatible output"`

---

### Task 4: `TupsonTypeResolver`

**Files:** Create `openapi4s/src/main/scala/ba/sake/openapi4s/tupson/TupsonTypeResolver.scala`

- [ ] **Step 1: Implement** (complete code):

```scala
package ba.sake.openapi4s
package tupson

import scala.meta._
import scala.meta.dialects.Scala34

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
      val core = resolveType(arr.schema, propertyName, parentTypeName, allowNullable = allowNullable, context, fallbackAnyType)
      if (arr.uniqueItems) t"Set[$core]" else t"Seq[$core]"
    case obj: SchemaDefinition.Obj          => namedTupleType(obj, context, fallbackAnyType)
    case e: SchemaDefinition.Enum           => literalUnionType(e.values.map(v => Lit.String(v): Lit))
    case e: SchemaDefinition.EnumLiterals   => literalUnionType(e.values.map(toLit))
    case c: SchemaDefinition.Const          => toLit(c.value)
    case SchemaDefinition.OneOf(schemas, _) => unionType(schemas, context, fallbackAnyType)
    case SchemaDefinition.AnyOf(schemas)    => unionType(schemas, context, fallbackAnyType)
    case SchemaDefinition.MapObj(valueSchemaOpt) =>
      valueSchemaOpt match {
        case Some(vs) => t"Map[String, ${resolveType(vs, propertyName, parentTypeName, allowNullable = allowNullable, context, fallbackAnyType)}]"
        case None     => t"Map[String, ${fallbackAnyType}]"
      }
    case other =>
      SchemaUtils.resolveType(other, propertyName, parentTypeName, allowNullable, context, fallbackAnyType)
  }

  private val IdentifierRegex = "[A-Za-z_][A-Za-z0-9_]*".r

  private def namedTupleType(obj: SchemaDefinition.Obj, context: String, fallbackAnyType: Type): Type = {
    val invalidName = obj.properties.find(p => IdentifierRegex.matches(p.name).not)
    if (obj.properties.isEmpty || invalidName.isDefined) {
      println(s"Cannot render anonymous object as named tuple (${invalidName.map(_.name).getOrElse("empty")}) [$context]. Falling back to ${fallbackAnyType.syntax}")
      fallbackAnyType
    } else {
      val typedParams = obj.properties.map { p =>
        Type.TypedParam(
          Type.Name(p.name),
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
      case Nil      => fallbackAnyType
      case h :: Nil => resolveType(h, None, None, allowNullable = true, context, fallbackAnyType)
      case multiple =>
        multiple
          .map(s => resolveType(s, None, None, allowNullable = true, context, fallbackAnyType))
          .reduceLeft[Type] { (acc, tpe) => t"$acc | $tpe" }
    }
}
```

(If `Type.TypedParam`'s 3-arg apply is missing on 4.9.9 — it isn't, structure output confirmed `(name, tpe, Nil)` — fall back to building via `t"(..${pats})"` after `param"..."` conversion; noted for implementer only if compile fails.)

- [ ] **Step 2: Run** `./mill openapi4s.compile` — Expected: PASS.
- [ ] **Step 3: Commit** — `git commit -m "feat: add TupsonTypeResolver (named tuples, literal unions, union types, maps)"`

---

### Task 5: `TupsonModelGenerator` rewrite

**Files:** Modify `openapi4s/src/main/scala/ba/sake/openapi4s/tupson/TupsonModelGenerator.scala`; Test: new `openapi4s/src/test/scala/ba/sake/openapi4s/TupsonGeneratorSuite.scala`

- [ ] **Step 1: Write failing tests** — `TupsonGeneratorSuite` generates from `tupson_features.yaml`/`tupson_features_31.yaml` (pattern: `OpenApiGeneratorSuite`) and asserts generated strings:

```scala
assert(modelsFile.contains("meta: (kind: String, age: Int)"))                     // anonymous object
assert(modelsFile.contains("status: \"available\" | \"pending\" | \"sold\""))      // inline enum
assert(modelsFile.contains("kind: \"dog\""))                                       // const
assert(modelsFile.contains("num: 1 | 2"))                                          // integer enum
assert(modelsFile.contains("extra: Map[String, String]"))                          // additionalProperties
assert(modelsFile.contains("type Pet = Cat | Dog"))                                // oneOf, no discriminator
assert(modelsFile.contains("@discriminator(\"pet_type\")"))                        // oneOf WITH discriminator kept
assert(modelsFile.contains("type X = 1 | 2"))                                      // named integer enum -> alias
```

- [ ] **Step 2: Run** — Expected: FAIL.
- [ ] **Step 3: Implement**
  - Property resolution: replace `SchemaUtils.resolveType(...)` call (`:52`) with `TupsonTypeResolver.resolveType(...)` (same args, keep `fallbackAnyType = t"JValue"`).
  - Delete the `adHocEnums` block (`:68-88`) and its splice (`:100`).
  - Rewrite the named-schema match:
    - `Obj` — unchanged (validators kept).
    - `Enum` — unchanged (Scala enum `derives JsonRW`).
    - `EnumLiterals` / `Const` — `List(q"type ${typeName} = ${TupsonTypeResolver.resolveType(schema, None, None, allowNullable = true, namedSchemaName, t"JValue")}")`.
    - `MapObj` — same alias pattern (`type X = Map[String, T]`).
    - `OneOf` / `AnyOf` — `discriminatorPropertyName match { case Some(d) => /* existing sealed trait + subtype recursion, Ref members only */ ; case None => union alias via TupsonTypeResolver }`. For `AnyOf` always the alias path.
    - `AllOf` — unchanged. `Arr` — unchanged (empty).
- [ ] **Step 4: Run** suite — Expected: PASS.
- [ ] **Step 5: Commit** — `git commit -m "feat: tupson backend generates named tuples, literal unions, union aliases, maps"`

---

### Task 6: `CirceModelGenerator` adaptations

**Files:** Modify `openapi4s/src/main/scala/ba/sake/openapi4s/circe/CirceModelGenerator.scala`

- [ ] **Step 1:** `OneOf` case — `withDiscriminator(Lit.String(oneOfSchema.discriminatorPropertyName.getOrElse("@type")))` (output identical to today).
- [ ] **Step 2:** Add a named `MapObj` case → `type X = Map[String, T]` alias via `SchemaUtils.resolveType(...)` (native circe maps — the agreed exception).
- [ ] **Step 3:** `AnyOf`/`Const`/`EnumLiterals` named → `List.empty` + `println` warning (preserves today's "skipped/unsupported" behavior).
- [ ] **Step 4: Run** `./mill openapi4s.test` — Expected: all existing circe suites green (assert byte-similar output in `OpenApiGeneratorSuite`).
- [ ] **Step 5: Commit** — `git commit -m "feat: circe backend handles new schema kinds without behavior change (+ maps)"`

---

### Task 7: Compile-verification suite

**Files:** Create `openapi4s/src/test/scala/ba/sake/openapi4s/TupsonCompilationSuite.scala` (copy the dotc-reflection pattern of `IronValidationCompilationSuite`)

- [ ] **Step 1: Implement** — two tests: `models = "tupson"` over `tupson_features.yaml` and `tupson_features_31.yaml`; compile generated sources via `dotty.tools.dotc.Main` with the test classpath (now includes tupson 0.20.0 + jawn). Reuse `generatedSourcesLog` for failure output.
- [ ] **Step 2: Run** — Expected: PASS (this is the guarantee that unions/literals/named tuples typecheck against real tupson).
- [ ] **Step 3: Commit** — `git commit -m "test: compile-verify tupson feature generation"`

---

### Task 8: Docs

**Files:** Modify `README.md`, `DEV.md`

- [ ] **Step 1:** README: drop "no anonymous objects" limitation; add "Tupson models backend" bullets (named tuples for anonymous objects, unions for ad hoc oneOf/anyOf, literal types for const/enums, maps); add a "Requirements" note: tupson backend needs **Scala 3.7+** and **tupson ≥ 0.20.0**; note union parse semantics (left-to-right, first match; use a discriminator for overlapping shapes).
- [ ] **Step 2:** `DEV.md` — add release reminder (bump README min versions).
- [ ] **Step 3: Commit** — `git commit -m "docs: document new tupson schema support and requirements"`

---

### Verification before completion

`./mill __.test` — all suites green; `./mill __.reformat` then re-test.
