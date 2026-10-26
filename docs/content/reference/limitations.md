---
title: Limitations
description: Current OpenAPI4s boundaries and generated TODOs
---

# {{ page.title }}

- Payload generation is JSON-only.
- JSON Schema input is model-only. It may be one `.json` file, a directory of schemas, or a single HTTP(S) URL. Remote schemas are fetched without authentication, resolve references against the document URL (not `$id`), and may only reference other HTTP(S) schemas.
- Incremental generation is additive; it does not automatically delete Scala code after contract removals or renames.
- http4s query parameters use typed DSL matchers. Unsupported decoder types such as UUIDs, dates and named enums receive one explicit `given QueryParamDecoder[T] = ???` to implement.
- Missing required or malformed http4s query parameters do not match the route, following http4s DSL behavior.
- Non-repeated array query encodings and query-parameter schema validation are not implemented.
- The sttp backend does not yet generate auth/security schemes, multipart bodies, streaming, or non-JSON content types.
