# openapi4s

OpenApi generators for Scala.

Here is a small video demo: https://youtu.be/kf0vGrlKNb8

## Features and Benefits
- incremental generator
  - doesn't touch the code that you added manually
  - **additive only**, adds new properties/methods/classes
- lenient parser+generator
  - if something is not supported it will still (mostly) work
  - you can adapt your openapi spec to work gradually

## Limitations
- JSON only

---

## Requirements

The **tupson** models backend generates code that needs:

- **Scala 3.7+** in your project
- **tupson >= 0.20.0** dependency (`ivy"ba.sake::tupson:0.20.0"`)

---

## Generators


### Circe models backend
- discriminated models (sealed traits / enums)
- enums (singleton enums)
- maps (`additionalProperties`)

### Tupson models backend
- discriminated models (sealed traits / enums)
- enums (singleton enums)
- **named tuples** for anonymous (inline) objects, e.g. `meta: (kind: String, age: Int)`
- **union types** for `oneOf`/`anyOf` without a discriminator, e.g. `type Pet = Cat | Dog`
- **literal types** for `const` and single-value enums, e.g. `kind: "dog"`, `count: 5`
- **literal unions** for inline and non-string enums, e.g. `status: "available" | "pending"`, `num: 1 | 2`
- maps (`additionalProperties`), e.g. `extra: Map[String, String]`

> When parsing union types, Tupson tries the members left-to-right and uses the
> first one that parses. For overlapping object shapes use a `discriminator` in
> your `oneOf` to get a sealed trait instead.

### Sharaf framework backend
Supports almost all features:
- controllers
- validations
- query params

### Http4s framework backend
Supports some features:
- routes (controllers)

TODO: query params, validation..  
Contributions welcome!

---

## Validation backends

You can optionally enable compile-time validation of your generated models with the `--validation` flag:

| `--validation` | Works with | What it does |
|---|---|---|
| `none` (default) | all | No validation. Models use plain Scala types. |
| `iron` | `--models circe` | Generates [Iron](https://github.com/iltotore/iron) newtypes (e.g. `Email`, `Username`) for constrained properties, plus a `models/Newtypes.scala` file. Models become *correct by construction* — decoding an invalid value fails. |
| `validson` | `--models tupson` | Enables runtime validation of Tupson models with [Validson](https://github.com/sake92/validson). |

Incompatible combinations (e.g. `--models tupson --validation iron`) are rejected at startup.

### Iron validation

```shell
--models circe --framework none --validation iron
```

Generated models use newtypes such as:

```scala
type Email = Email.T
object Email extends RefinedType[String, Match["^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$"]]
```

You need to add these dependencies to your own build:

```scala
ivy"io.github.iltotore::iron:3.0.2"
ivy"io.github.iltotore::iron-circe:3.0.2"
```

---

## Usage

### Mill plugin

See https://github.com/sake92/mill-openapi4s

### CLI

You can use `openapi4s-cli` with Coursier launcher to generate your sources:

```shell
cs launch ba.sake::openapi4s-cli:0.7.0 -M ba.sake.openapi4s.cli.OpenApi4sMain -- \
  --models tupson \
  --framework sharaf \
  --validation none \
  --url openapi.json \
  --baseFolder src \
  --basePackage com.example
```

You can now combine model and framework generation independently:

```shell
# circe models + http4s routes
--models circe --framework http4s

# circe models + sharaf controllers
--models circe --framework sharaf

# models only
--models tupson --framework none

# framework only (expects existing com.example.models)
--models none --framework http4s
```
