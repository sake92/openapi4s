# Sttp Client Generation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add an sttp-client4 generation backend to openapi4s, selectable per-tag via a new `--tags` flag that currently filters clients only.

**Architecture:** New `ClientBackend` dimension parallel to `FrameworkBackend` (`--client sttp`, default `none`). `SttpClientGenerator` mirrors `SharafGenerator`: scala.meta quasiquotes (Scala34 dialect), one `TagClient` class per tag in `${basePackage}.clients`, regenesca `GeneratedFileSource` output. Tag filter applied only to the definition passed to the client backend.

**Tech Stack:** Scala 2.13 (generator), scala.meta + regenesca (existing), generated code targets Scala 3 + sttp client4, circe/tupson JSON. Compile verification via scala-cli (`CompilationTestUtils`).

**Spec:** `docs/superpowers/specs/2026-08-27-sttp-client-generation-design.md`

## Global Constraints

- Generated code uses `scala.meta.dialects.Scala34` (like existing generators)
- Lenient philosophy: unsupported schema → warn + skip param / body; never fail generation
- Additive-only: filtering means "don't generate", never delete
- `--tags` currently affects **only** client generation (models/framework/validation untouched)
- JSON only, `application/json` content type only
- sttp client4 shape: `Request[Either[ResponseException[String], T]]`
- Param/identifier names sanitized with existing `ScalaIdents.termName`
- Compile-verification suites use `CompilationTestUtils.compileGenerated(base, "3.7.3", deps)` (scala-cli, no build.mill test-dep changes)

---

### Task 0: Write spec + plan docs, commit

**Files:**
- Create: `docs/superpowers/specs/2026-08-27-sttp-client-generation-design.md`
- Create: `docs/superpowers/plans/2026-08-27-sttp-client-generation.md`

- [ ] Write the approved design (architecture, client shape, selection, leniency, testing, out-of-scope, verified client4 facts) to the spec file
- [ ] Write this plan to the plans file
- [ ] Commit: `git add docs/superpowers && git commit -m "docs: sttp client generation design + plan"`

---

### Task 1: Parse servers + header params into the definition

**Files:**
- Modify: `openapi4s/src/main/scala/ba/sake/openapi4s/OpenApiDefinition.scala`
- Modify: `openapi4s/src/main/scala/ba/sake/openapi4s/PathDefinition.scala`
- Modify: `openapi4s/src/main/scala/ba/sake/openapi4s/PathDefinitionsResolver.scala`
- Test: `openapi4s/src/test/scala/ba/sake/openapi4s/PathDefinitionsResolverSuite.scala`
- Test: `openapi4s/src/test/scala/ba/sake/openapi4s/OpenApiDefinitionSuite.scala`
- Test resource: `openapi4s/src/test/resources/sttp_client.yaml` (new fixture)

**Interfaces:**
- Produces: `OpenApiDefinition(namedSchemaDefinitions, pathDefinitions, servers: List[String] = List.empty)`; `case class HeaderParam(name: String, schema: SchemaDefinition, required: Boolean)`; `PathDefinition(..., headerParams: List[HeaderParam], ...)`

- [ ] **Step 1: Write failing tests** — `PathDefinitionsResolverSuite`: parse new fixture, assert servers + header params (required + optional) + object-schema header dropped (lenient)
- [ ] **Step 2: Run** — `./mill openapi4s.test.testOnly ba.sake.openapi4s.PathDefinitionsResolverSuite` → FAIL
- [ ] **Step 3: Implement** — add fields + parsing (see plan/spec for exact code)
- [ ] **Step 4: Run** — PASS (also `OpenApiDefinitionSuite`)
- [ ] **Step 5: Commit** — `feat: parse servers and header params into OpenApiDefinition`

---

### Task 2: `ClientBackend` dimension + minimal `SttpClientGenerator`

**Files:**
- Create: `openapi4s/src/main/scala/ba/sake/openapi4s/ClientBackend.scala`
- Create: `openapi4s/src/main/scala/ba/sake/openapi4s/sttp/SttpClientGenerator.scala`
- Test: `openapi4s/src/test/scala/ba/sake/openapi4s/sttp/SttpClientGeneratorSuite.scala`

**Interfaces:**
- Consumes: `PathDefinition` (with `headerParams`), `OpenApiDefinition.servers`, `ScalaIdents.termName`
- Produces: `ClientBackendId` (`NoClient`, `Sttp`) + `fromString`; `ClientBackend` trait (`id`, `supportedModelIds: Set[ModelBackendId]`, `generator(config, openapiDefinition, modelContract): OpenApiGenerator`); `ClientBackend.byId`; `SttpClientGenerator(config, openApiDefinition, modelContract)` with `generate(): Seq[GeneratedFileSource]`

- [ ] **Step 1: Verify client4 APIs** via ctx7: `asJson`/`asString`/`ResponseAs.deserializeRightWithError`/uri interpolator Option handling/`.header`
- [ ] **Step 2: Write failing tests** — `SttpClientGeneratorSuite`: per-tag files (`PetClient`, `StoreClient`, `DefaultClient`), server consts, method sigs, uri/query/header patterns
- [ ] **Step 3: Implement** — `ClientBackend.scala` (mirror `FrameworkBackend`; sttp supportedModelIds = Circe+Tupson) + `SttpClientGenerator` (group by tag, `TagClient`, method naming with operationId fallback + dedup, path/query/header params, `ScalaIdents.termName`)
- [ ] **Step 4: Run** — suite PASS; `SharafGeneratorSuite` unchanged
- [ ] **Step 5: Commit** — `feat: add ClientBackend dimension and SttpClientGenerator (paths, query, headers)`

---

### Task 3: Bodies, JSON helpers, empty responses, leniency

**Files:**
- Modify: `openapi4s/src/main/scala/ba/sake/openapi4s/sttp/SttpClientGenerator.scala`
- Test: extend `SttpClientGeneratorSuite`

- [ ] **Step 1: Write failing tests** — circe body/response patterns; tupson `JsonSupport.asJson` + `.body(x.toJson)` + contentType; 204 → Unit; missing operationId fallback; unsupported body → skip with warning
- [ ] **Step 2: Run** — FAIL
- [ ] **Step 3: Implement** — req body (`SchemaUtils.resolveType`, fallback `JValue`), res body asJson (circe) / `JsonSupport.asJson` (tupson), `JsonSupport.scala` helper file for tupson, Unit for empty responses, try/catch `UnsupportedSchemaException` like `SharafGenerator`
- [ ] **Step 4: Run** — PASS
- [ ] **Step 5: Commit** — `feat: sttp client bodies, tupson JsonSupport, empty responses, lenient fallbacks`

---

### Task 4: Wire into `OpenApiWriter` + tag filtering

**Files:**
- Modify: `openapi4s/src/main/scala/ba/sake/openapi4s/OpenApiWriter.scala`
- Test: `openapi4s/src/test/scala/ba/sake/openapi4s/OpenApiGeneratorSuite.scala` (extend)

- [ ] **Step 1: Write failing tests** — `Config(client="sttp", tags=Some(List("Pet")))` → only PetClient generated, models all present; `client="bogus"` throws listing `'sttp', 'none'`; models=none+client=sttp warns
- [ ] **Step 2: Run** — FAIL
- [ ] **Step 3: Implement** — Config fields (`client`, `tags`), apply() validation + nothing-to-generate check, write() filtered clientDefinition + clientSources in pipeline + printlns
- [ ] **Step 4: Run** — PASS
- [ ] **Step 5: Commit** — `feat: wire client backend into OpenApiWriter with tag filtering`

---

### Task 5: CLI flags

**Files:**
- Modify: `cli/src/main/scala/ba/sake/openapi4s/cli/OpenApi4sMain.scala`

- [ ] **Step 1: Implement** — `--client` (default `none`), `--tags` (comma-separated → `Option[List[String]]`)
- [ ] **Step 2: Verify** — `./mill cli.compile`; smoke-run CLI on petstore with `--client sttp --tags Pet`
- [ ] **Step 3: Commit** — `feat: add --client and --tags CLI flags`

---

### Task 6: Compile-verification suite

**Files:**
- Create: `openapi4s/src/test/scala/ba/sake/openapi4s/SttpClientCompilationSuite.scala`

- [ ] **Step 1: Find latest client4 version** — maven search `g:com.softwaremill.sttp.client4`
- [ ] **Step 2: Write test** — mirror `IronValidationCompilationSuite`/`TupsonCompilationSuite` (scala-cli, `munitTimeout` 10 min): circe + tupson client generation from `sttp_client.yaml`, compile via `CompilationTestUtils.compileGenerated` with sttp core/circe/tupson deps; also a `--tags` subset run
- [ ] **Step 3: Run** — `./mill openapi4s.test.testOnly ba.sake.openapi4s.SttpClientCompilationSuite` → PASS
- [ ] **Step 4: Commit** — `test: compile-verify sttp client generation (circe + tupson)`

---

### Task 7: Docs + full verification

**Files:**
- Modify: `README.md`
- Modify: `DEV.md`

- [ ] Update README (Generators: "Sttp client backend"; Requirements; usage example) + DEV.md (mill-openapi4s follow-up note)
- [ ] Run `./mill __.reformat` and `./mill __.test` — all green
- [ ] Commit — `docs: document sttp client generation`
