# openapi4s

OpenAPI generators for Scala 3. Give it an OpenAPI 3.0/3.1 spec and it generates idiomatic Scala 3 models, HTTP routes/controllers, and HTTP clients. It can also generate validators for your models.
It also accepts local JSON Schema files or directories for model-only generation.

[![CI](https://github.com/sake92/openapi4s/actions/workflows/ci.yml/badge.svg)](https://github.com/sake92/openapi4s/actions/workflows/ci.yml)

Small video demo: https://youtu.be/kf0vGrlKNb8

Documentation: https://sake92.github.io/openapi4s

## Features

- **incremental generator**
  - doesn't touch the code that you added manually
  - **additive only** — adds new properties/methods/classes
- **lenient parser + generator**
  - if something is not supported it will still (mostly) work
  - you can adapt your OpenAPI spec to work gradually

## Why OpenAPI4s?

OpenAPI4s is for teams that want an OpenAPI contract to remain the source of truth without treating generated Scala as disposable output. It generates idiomatic Scala 3 models, validation types, server routes and clients, then uses semantic Scala source merging to apply later contract changes while preserving handwritten implementations. New fields, constraints and endpoints can be regenerated into an existing project.

This differs from Scala-first libraries such as Tapir, where Scala owns the API and OpenAPI is derived from it, and from template generators such as Guardrail or OpenAPI Generator, which overwrite generated transport files and keep user logic behind a separate interface. OpenAPI4s is the strongest fit when the requirements are **contract-first OpenAPI, modern Scala 3, readable generated code and safe incremental regeneration**. Choose a Scala-first library instead when Scala—not OpenAPI—should own the contract.

## Usage

### Snapshots

After tests pass, pushes to `main` between release tags publish snapshots of both
`openapi4s` and `openapi4s-cli`. Versions use Deder’s git-based versioning,
for example `0.9.1-SNAPSHOT` after release `0.9.0`. Find the version in the
CI job's **Check publishing versions** output.

Add `https://central.sonatype.com/repository/maven-snapshots` to your build's
repositories to use a library snapshot. For the CLI:

```shell
cs launch --scala-version 2.13.16 \
  -r https://central.sonatype.com/repository/maven-snapshots \
  ba.sake::openapi4s-cli:0.9.1-SNAPSHOT \
  -M ba.sake.openapi4s.cli.OpenApi4sMain -- --help
```

Replace the example version with the snapshot you want to use. Tagged releases
continue to publish to Maven Central.

### CLI

All runs generate models. Add `--framework` to generate a server and `--client` to
generate a client. The examples below are self-contained; replace the spec path and
`--basePackage` with your own values.

Generate a server (models + routes/controllers):

```shell
cs launch --scala-version 2.13.16 \
  ba.sake::openapi4s-cli:0.9.0 -M ba.sake.openapi4s.cli.OpenApi4sMain -- \
  --models circe \
  --framework http4s \
  --url openapi.yaml \
  --baseFolder src/main/scala \
  --basePackage com.example.api
```

Generate a client (models + sttp client):

```shell
cs launch --scala-version 2.13.16 \
  ba.sake::openapi4s-cli:0.9.0 -M ba.sake.openapi4s.cli.OpenApi4sMain -- \
  --models tupson \
  --client sttp \
  --url openapi.yaml \
  --baseFolder src/main/scala \
  --basePackage com.example.api
```

Generate models only from a `my_schemas` folder:

```shell
cs launch --scala-version 2.13.16 \
  ba.sake::openapi4s-cli:0.9.0 -M ba.sake.openapi4s.cli.OpenApi4sMain -- \
  --models tupson \
  --url my_schemas \
  --baseFolder src/main/scala \
  --basePackage com.example.models
```

| Flag | Default | Description |
|---|---|---|
| `--models` | `tupson` | Model backend: `circe` or `tupson` |
| `--framework` | *(none)* | Server framework backend: `http4s` or `sharaf`. If unset, no server is generated. |
| `--validation` | `none` | Validation backend: `none`, `iron` or `validson` |
| `--client` | *(none)* | Client backend: `sttp`. If unset, no client is generated. |
| `--tags` | *(none)* | Comma-separated tags to generate server and client code for (case-insensitive, first tag of an operation). |
| `--excludeTags` | *(none)* | Comma-separated tags to skip for server and client. Wins over `--tags`. |
| `--serverTags`, `--serverExcludeTags` | *(none)* | Server-only overrides of `--tags` / `--excludeTags`. |
| `--clientTags`, `--clientExcludeTags` | *(none)* | Client-only overrides of `--tags` / `--excludeTags`. |
| `--url` | `openapi.json` | OpenAPI spec URL/file path, or a local JSON Schema `.json` file/directory |
| `--baseFolder` | `src/main/scala` | Folder for generated sources |
| `--basePackage` | *(required)* | Package for generated sources |

You can combine or customize the backends independently:

```shell
# circe models + http4s routes
--models circe --framework http4s

# tupson models + sharaf controllers
--models tupson --framework sharaf

# models only from an OpenAPI spec
--models tupson

# models from one JSON Schema file or a directory of .json schemas
--models tupson --url schemas/

# tupson models + sttp client for all tags
--models tupson --client sttp

# circe models + sttp client, only for tags Pet and Store (also filters the server, if any)
--models circe --client sttp --tags Pet,Store

# models + sharaf server + sttp client in one run
--models tupson --framework sharaf --client sttp
```

Generated files land in `<baseFolder>/<basePackage path>/`:

```
src/main/scala/com/example/
├── models/          # one file per schema (+ Newtypes.scala for iron)
├── controllers/     # sharaf controllers (only with --framework sharaf)
├── routes/          # http4s routes (only with --framework http4s)
└── clients/         # sttp clients (only with --client sttp)
```

### Build plugins

The [Mill plugin](https://github.com/sake92/mill-openapi4s) and
[sbt plugin](https://github.com/sake92/sbt-openapi4s) provide equivalent settings.

## Backends

Openapi4s is a matrix of independent backends — pick one model backend, one server backend, and optionally a validation backend.

### Model backends

| Feature | `circe` | `tupson` |
|---|---|---|
| Discriminated `oneOf` → sealed trait / enum | ✅ | ✅ |
| Enums (singleton enums) | ✅ | ✅ |
| `additionalProperties` → `Map` | ✅ | ✅ |
| Inline (anonymous) objects → named tuples, e.g. `meta: (kind: String, age: Int)` | — | ✅ |
| Undiscriminated `oneOf`/`anyOf` → union types, e.g. `type Pet = Cat \| Dog` | — | ✅ |
| `const` and single-value enums → literal types, e.g. `kind: "dog"`, `count: 5` | — | ✅ |
| Inline and non-string enums → literal types, e.g. `status: "available" \| "pending"`, `num: 1 \| 2` | — | ✅ |

> When parsing union types, Tupson tries the members left-to-right and uses the
> first one that parses. For overlapping object shapes use a `discriminator` in
> your `oneOf` to get a sealed trait instead.

Circe models honor explicit `discriminator.mapping` values, such as `cat` → `Cat`,
with schema names as the default for unmapped cases. Mapping several wire values
to the same case is currently unsupported and fails generation with an error.

### Server backends

| Feature | `sharaf` | `http4s` |
|---|---|---|
| Controllers / routes | ✅ | ✅ |
| Query params | ✅ | ✅ |

`sharaf` works with `tupson` models, `http4s` works with `circe` models.
Other model/framework combos only print a warning — generated sources may need manual adjustments.

### Client backends

| Feature | `sttp` |
|---|---|
| HTTP clients — one `XClient` class per tag | ✅ |
| Path, query and header params | ✅ |
| JSON request/response bodies (`application/json`) | ✅ |
| Spec `servers` → `server1`, `server2`, ... constants in the client companion | ✅ |
| Selective generation via `--tags` / `--excludeTags` (server and client; models are always generated) | ✅ |
| Auth/security schemes, multipart, streaming, non-JSON content types | TODO — contributions welcome |

`sttp` works with both `tupson` and `circe` models. tupson clients use `ba.sake.sttp.tupson.asJson` natively. Generated example:

```scala
class PetClient(baseUrl: String) {
  def getPetById(petId: Long): Request[Either[ResponseException[String], Pet]] =
    basicRequest.get(uri"$baseUrl/pet/$petId").response(asJson[Pet])
}
```

### Validation backends

Optionally enable validation of your generated models with `--validation`:

| `--validation` | Works with | What it does |
|---|---|---|
| `none` (default) | all | No validation backend. (Tupson models always get Validson constraint validators.) |
| `iron` | `--models circe` | Generates [Iron](https://github.com/iltotore/iron) newtypes (e.g. `Email`, `Username`) for constrained properties, plus a `models/Newtypes.scala` file. Models become *correct by construction* — decoding an invalid value fails. |
| `validson` | `--models tupson` | Constraint validation (min/max/length/pattern) with [Validson](https://github.com/sake92/validson) via generated `given Validator[X]` instances. |

Incompatible validation/model combos (e.g. `--models tupson --validation iron`) are rejected at startup.

## Requirements

Add the dependencies that the generated code needs to your own build. Generated-code compilation tests run on Scala 3.9.0:

| Generated | Requirements |
|---|---|
| `tupson` models | Scala 3.9+, `ivy"ba.sake::tupson:0.31.0"`, `ivy"ba.sake::validson:0.20.0"` |
| `circe` models | Scala 3, `ivy"io.circe::circe-core:0.14.17"`, `ivy"io.circe::circe-generic:0.14.17"` |
| `sharaf` controllers | `ivy"ba.sake::sharaf:0.9.3"` |
| `http4s` routes | `ivy"org.http4s::http4s-dsl:0.23.38"` (with circe models also `ivy"org.http4s::http4s-circe:0.23.38"`) |
| `iron` validation | Scala 3.9+, `ivy"io.github.iltotore::iron:3.4.1"`, `ivy"io.github.iltotore::iron-circe:3.4.1"` |
| `sttp` clients | Scala 3.9+, `ivy"com.softwaremill.sttp.client4::core:4.1.0"` (with circe models also `ivy"com.softwaremill.sttp.client4::circe:4.1.0"`; with tupson models also `ivy"ba.sake::tupson-sttp:0.31.0"`) |

## Limitations

- JSON only — no XML, protobuf, etc.
- JSON Schema input is local and model-only: use a `.json` file or directory. Directory references may target another schema root or a top-level `$defs` entry (`other.json#/$defs/Name`).
- `http4s` query params use typed DSL matchers. Optional scalars bind to `Option`; repeated array
  parameters bind to sequences (empty when an optional array is absent). Built-in decoders are inferred;
  other types (such as UUIDs, dates, and named enums) get one `given QueryParamDecoder[T] = ???`
  with a TODO comment per type. Fill these in before using the corresponding routes.
  Missing required or malformed parameters do not match the route, following http4s DSL behavior.
  Inline enums use primitive types; schema validation and non-repeated array encodings are not implemented.
- `sttp` client backend: no auth/security schemes, multipart, streaming, or non-JSON content types yet

## Development

See [DEV.md](DEV.md) for development notes and the release process.
