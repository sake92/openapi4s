# Sttp Client Generation Design

Date: 2026-08-27
Status: Approved

## Goal

Add an HTTP client generation backend for [sttp](https://sttp.softwaremill.com/)
to openapi4s. Users can generate one client class per OpenAPI tag
(e.g. `PetClient`), with typed methods per operation, and optionally restrict
generation to a subset of tags. Selection via tags currently applies **only**
to client generation.

## Prior art: openapi-generator

- `--global-property apis=Pet,Store` filters operations grouped by their
  (first) tag — only selected tags get an API file. `--global-property
  models=...` filters `components/schemas` by name. The two filters are
  independent; filtering apis does not prune models.
- Its scala-sttp generator emits one class per tag (`PetClient`), methods
  named by `operationId`, each returning a sttp `Request` builder with
  `baseUrl` as a constructor argument.

## Architecture

A new `ClientBackend` dimension, parallel to the existing `FrameworkBackend`:

- `sealed trait ClientBackendId`: `NoClient`, `Sttp` (+ `fromString`).
- `trait ClientBackend`: `id`, `supportedModelIds`, and
  `generator(config, openapiDefinition, modelContract): OpenApiGenerator`.
- `ClientBackend.byId: Map[ClientBackendId, ClientBackend]`.
- New CLI flag `--client sttp` (default `none`), combined freely with
  `--models` (circe or tupson) and `--framework`.

`OpenApiWriter.Config` gains `client: String = "none"` and
`tags: Option[List[String]] = None`. `OpenApiWriter.write()` runs the client
backend after the framework backend and merges its sources into the same
regenesca pipeline.

## Generated client shape

For each tag `Pet` (grouped by `PathDefinition.getTag`, i.e. first tag or
`Default`), one file `clients/PetClient.scala` in package
`${basePackage}.clients`:

```scala
// generated with OpenApi4s
package com.example.clients {
  import sttp.client4.*
  import com.example.models.*
  // circe:  import sttp.client4.circe.*
  // tupson: import com.example.clients.JsonSupport.*

  object PetClient {
    val server1: String = "http://petstore.swagger.io/v1" // from spec `servers`
  }

  class PetClient(baseUrl: String) {
    def getPetById(petId: Long): Request[Either[ResponseException[String], Pet]] =
      basicRequest
        .get(uri"$baseUrl/pet/$petId")
        .response(asJson[Pet])
  }
}
```

### Decisions

1. **Target**: sttp **client4** (user choice). Generated code imports
   `sttp.client4.*`. `Request[T]` has no capability parameter, so any backend
   (sync, Future, IO, ZIO...) can send it.
2. **Method return type**: `Request[Either[ResponseException[String], T]]` —
   builder style (like openapi-generator's scala-sttp). Caller supplies the
   backend. Verified: client4 `asJson[B]` returns
   `ResponseAs[Either[ResponseException[String], B]]`; HTTP errors are
   `HttpError(String)`, decode failures `DeserializationException`. The
   client3 `ResponseException[+HE, +DE]` covariance precedent means widening
   to `Exception` is sound, but client4's single-parameter `ResponseException`
   is used directly.
3. **Naming**: class `TagClient` via `CaseUtils.toCamelCase(tag, true, '_')`,
   mirroring `TagController`. Methods from `operationId` (camelCased); if
   missing, derive from method + path segments, de-duplicated with a numeric
   suffix. Parameter names sanitized via `ScalaIdents.termName` (existing
   helper).
4. **baseUrl**: required constructor arg. Spec `servers` are parsed into
   `OpenApiDefinition.servers: List[String]` and exposed as `server1`,
   `server2`, ... vals in each client companion. No servers in the spec →
   no constants.
5. **JSON**:
   - circe models: `import sttp.client4.circe.*`, `.body(model)` and
     `asJson[T]` as-is.
   - tupson models: sttp has no tupson module, so a single generated
     `clients/JsonSupport.scala` (tupson only) provides
     `def asJson[T: JsonRW]: ResponseAs[Either[ResponseException[String], T]]`
     implemented via `asString.mapWithMetadata(ResponseAs.deserializeRightWithError(...))`
     wrapping `body.parseJson[T]` (tupson's parse API). Request bodies:
     `.body(value.toJson).contentType("application/json")`.
6. **Params**:
   - Path params interpolated into the uri: `uri"$baseUrl/pets/$id"`.
   - Query params: required `T`, optional `Option[T]`; interpolated as
     `?name=$name` (client4 uri interpolator omits `None` values).
   - Header params (newly parsed into `PathDefinition`): required `T` →
     `.header("name", v)`; optional → block pattern
     `opt.fold(req)(v => req.header("name", v))`.
   - Unsupported param schemas → warning + drop param (lenient, like
     `SharafGenerator`).
7. **Bodies**: JSON request body via `.body(...)`; 2xx response body via
   `asJson[T]`; no 2xx body / 204 → `Request[Either[ResponseException[String], Unit]]`
   via `asString.map(_.map(_ => ()))`.
8. **Imports**: `GenerationImports.modelWildcardImport(config.basePackage)` +
   sttp imports + backend-specific JSON imports.

## Selection (`--tags`)

- CLI: `--tags Pet,Store` (comma-separated). Absent → all tags.
- Applied **only to the client backend**: `OpenApiWriter` computes a filtered
  copy of the definition (`pathDefinitions.defs.filter(d => tags.contains(d.getTag))`)
  and passes it to the client generator. Models, validation, and framework
  generation are untouched → partial clients can never break compilation of
  the rest.
- Additive/incremental: filtering means "don't generate", never delete;
  regenesca already never removes previously generated files.

## Leniency & edge cases

- Unsupported schemas: warn (`println(e.toString)`) and skip the param/body,
  exactly like `SharafGenerator`; generation always succeeds.
- No `operationId` → derived name from method + path, de-duplicated.
- No 2xx body → `Unit` response.
- No servers in spec → no `server*` constants.

## Testing

- `SttpClientGeneratorSuite` (munit): new fixture `sttp_client.yaml`
  (servers, tags Pet/Store/Default, path/query/header params, JSON bodies,
  204, missing operationId, unsupported schema) — assert generated file
  names, method signatures, server constants, leniency behavior.
- `SttpClientCompilationSuite` (munit, scala-cli based like
  `TupsonCompilationSuite`): generate circe+tupson clients from the fixture
  and compile with `CompilationTestUtils.compileGenerated(base, "3.7.3",
  deps)` using `com.softwaremill.sttp.client4::core` (+ `::circe`,
  `ba.sake::tupson`) — proves generated code typechecks against real jars.
- Writer-level tests for tag filtering (models unaffected) and
  backend-combination warnings/errors.
- CLI smoke test: `--client sttp --tags Pet` on petstore.

## Out of scope (v1)

- Auth/security schemes (needs `securitySchemes` parsing — separate feature)
- Non-JSON content types, multipart, streaming
- Path-level server overrides
- `--tags` affecting models or framework generation (documented as
  client-only for now; flag name left generic so it can widen later)
- mill-openapi4s plugin support (separate repo, follow-up)

## Verified sttp client4 facts

- `asJson[B]: ResponseAs[Either[ResponseException[String], B]]` (circe
  module: `Left(HttpError(String))` on non-2xx, `Left(DeserializationException)`
  on decode failure).
- uri interpolator supports optional parameters — `None` values are omitted.
- `Request[T]` carries no capability type in client4; sendable with any
  backend.
