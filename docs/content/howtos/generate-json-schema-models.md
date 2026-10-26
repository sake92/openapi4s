---
layout: howto.html
title: Generate models from JSON Schema
description: Generate Scala 3 models from a JSON Schema file or directory
---

# {{ page.title }}

Pass a JSON Schema file to the CLI and choose the model backend you use in your application:

```shell
cs launch --scala-version 2.13.16 ba.sake::openapi4s-cli:0.9.0 \
  -M ba.sake.openapi4s.cli.OpenApi4sMain -- \
  --models circe \
  --url schemas/person.json \
  --baseFolder src/main/scala \
  --basePackage com.example.models
```

The generated models are written below `src/main/scala/com/example/models/models`. Use `--models tupson` instead when your application uses Tupson.

To generate from a collection of schemas, point `--url` at its directory:

```shell
cs launch --scala-version 2.13.16 ba.sake::openapi4s-cli:0.9.0 \
  -M ba.sake.openapi4s.cli.OpenApi4sMain -- \
  --models circe \
  --url schemas/ \
  --baseFolder src/main/scala \
  --basePackage com.example.models
```

The directory is walked recursively and every `.json` file is treated as a schema document. Keep only JSON Schema documents in that tree. Relative `$ref` values may point to another schema in the tree:

```json
{
  "title": "Customer",
  "type": "object",
  "required": ["address"],
  "properties": {
    "address": { "$ref": "address.json" }
  }
}
```

Give each top-level schema a unique `title`; otherwise its filename becomes the model name. Definitions in `$defs` are generated as additional models.

JSON Schema input generates models only. Do not pass `--framework` or `--client`; those options require an OpenAPI document. For an HTTP(S) schema URL, OpenAPI4s fetches schemas without authentication and remote schemas may reference only HTTP(S) schemas. See the [limitations reference](/reference/limitations.html) for the current boundary.
