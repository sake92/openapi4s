# openapi4s

OpenAPI generators for Scala 3. Give it an OpenAPI 3.0/3.1 spec and it generates idiomatic Scala 3 models and HTTP routes/controllers.

[![CI](https://github.com/sake92/openapi4s/actions/workflows/ci.yml/badge.svg)](https://github.com/sake92/openapi4s/actions/workflows/ci.yml)

Small video demo: https://youtu.be/kf0vGrlKNb8

## Features

- **incremental generator**
  - doesn't touch the code that you added manually
  - **additive only** — adds new properties/methods/classes
- **lenient parser + generator**
  - if something is not supported it will still (mostly) work
  - you can adapt your OpenAPI spec to work gradually

## Backends

Openapi4s is a matrix of independent backends — pick one model backend, one framework backend, and optionally a validation backend.

### Model backends

| Feature | `circe` | `tupson` |
|---|---|---|
| Discriminated `oneOf` → sealed trait / enum | ✅ | ✅ |
| Enums (singleton enums) | ✅ | ✅ |
| `additionalProperties` → `Map` | ✅ | ✅ |
| Inline (anonymous) objects → named tuples, e.g. `meta: (kind: String, age: Int)` | — | ✅ |
| `oneOf`/`anyOf` without discriminator → union types, e.g. `type Pet = Cat \| Dog` | — | ✅ |
| `const` and single-value enums → literal types, e.g. `kind: "dog"`, `count: 5` | — | ✅ |
| Inline and non-string enums → literal unions, e.g. `status: "available" \| "pending"`, `num: 1 \| 2` | — | ✅ |
| min/max/length/pattern constraints → Validson validators | — | ✅ |

> When parsing union types, Tupson tries the members left-to-right and uses the
> first one that parses. For overlapping object shapes use a `discriminator` in
> your `oneOf` to get a sealed trait instead.

### Framework backends

| Feature | `sharaf` | `http4s` |
|---|---|---|
| Controllers / routes | ✅ | ✅ |
| Query params | ✅ | TODO — contributions welcome |
| Validation | ✅ | TODO — contributions welcome |

`sharaf` works with `tupson` models, `http4s` works with `circe` models.
Other model/framework combos only print a warning — generated sources may need manual adjustments.

### Client backends

| Feature | `sttp` |
|---|---|
| HTTP clients — one `XClient` class per tag | ✅ |
| Path, query and header params | ✅ |
| JSON request/response bodies (`application/json`) | ✅ |
| Spec `servers` → `server1`, `server2`, ... constants in the client companion | ✅ |
| Selective generation via `--tags` (applies to clients only) | ✅ |
| Auth/security schemes, multipart, streaming, non-JSON content types | TODO — contributions welcome |

`sttp` works with both `tupson` and `circe` models. Generated example:

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

### Iron example

```shell
--models circe --validation iron
```

Generated models use newtypes such as:

```scala
type Email = Email.T
object Email extends RefinedType[String, Match["^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$"]]
```

## Requirements

Add the dependencies that the generated code needs to your own build:

| Generated | Requirements |
|---|---|
| `tupson` models | Scala 3.7+, `ivy"ba.sake::tupson:0.30.0"`, `ivy"ba.sake::validson:0.19.0"` |
| `circe` models | Scala 3, `ivy"io.circe::circe-core:0.14.10"`, `ivy"io.circe::circe-generic:0.14.10"` |
| `sharaf` controllers | `ivy"ba.sake::sharaf:0.9.3"` |
| `http4s` routes | `ivy"org.http4s::http4s-dsl:0.23.x"` (with circe models also `ivy"org.http4s::http4s-circe:0.23.x"`) |
| `iron` validation | `ivy"io.github.iltotore::iron:3.0.2"`, `ivy"io.github.iltotore::iron-circe:3.0.2"` |
| `sttp` clients | `ivy"com.softwaremill.sttp.client4::core:4.0.26"` (with circe models also `ivy"com.softwaremill.sttp.client4::circe:4.0.26"`, with tupson models also `ivy"ba.sake::tupson-sttp:0.30.0"`) |

## Usage

### CLI

Run the CLI with the Coursier launcher:

```shell
cs launch ba.sake::openapi4s-cli:0.7.0 -M ba.sake.openapi4s.cli.OpenApi4sMain -- \
  --models tupson \
  --framework sharaf \
  --validation validson \
  --url openapi.json \
  --baseFolder src/main/scala \
  --basePackage com.example
```

| Flag | Default | Description |
|---|---|---|
| `--models` | `tupson` | Model backend: `circe` or `tupson` |
| `--framework` | *(none)* | Server framework backend: `http4s` or `sharaf`. If unset, no server is generated. |
| `--validation` | `none` | Validation backend: `none`, `iron` or `validson` |
| `--client` | *(none)* | Client backend: `sttp`. If unset, no client is generated. |
| `--tags` | *(none)* | Comma-separated tags to generate clients for. Currently applies only to clients. |
| `--url` | `openapi.json` | OpenAPI spec — a URL or a file path |
| `--baseFolder` | `src/main/scala` | Folder for generated sources |
| `--basePackage` | *(required)* | Package for generated sources |

You can combine the backends independently:

```shell
# circe models + http4s routes
--models circe --framework http4s

# tupson models + sharaf controllers
--models tupson --framework sharaf

# models only
--models tupson

# tupson models + sttp client for all tags
--models tupson --client sttp

# circe models + sttp client, only for tags Pet and Store (applies to clients only)
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

### Mill plugin

There is also a Mill plugin: https://github.com/sake92/mill-openapi4s

## Limitations

- JSON only — no XML, protobuf, etc.
- `http4s` backend: routes only; query params and validation are not implemented yet
- `sttp` client backend: no auth/security schemes, multipart, streaming, or non-JSON content types yet

## Development

See [DEV.md](DEV.md) for development notes and the release process.
