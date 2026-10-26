---
title: CLI
description: OpenAPI4s command-line options
---

# {{ page.title }}

All runs generate models. Add `--framework` for a server or `--client` for a client.

| Flag | Default | Description |
|---|---|---|
| `--models` | `tupson` | Model backend: `circe` or `tupson` |
| `--framework` | none | Server backend: `http4s` or `sharaf` |
| `--validation` | `none` | Validation backend: `none`, `iron` or `validson` |
| `--client` | none | Client backend: `sttp` |
| `--tags` | none | Comma-separated operation tags to include |
| `--excludeTags` | none | Comma-separated tags to exclude; wins over `--tags` |
| `--serverTags`, `--serverExcludeTags` | none | Server-only tag overrides |
| `--clientTags`, `--clientExcludeTags` | none | Client-only tag overrides |
| `--url` | `openapi.json` | OpenAPI URL/path, JSON Schema HTTP(S) URL, file, or directory |
| `--baseFolder` | `src/main/scala` | Generated source directory |
| `--basePackage` | required | Package for generated sources |

Generated sources land below `<baseFolder>/<basePackage path>/` in `models`, `controllers`, `routes` and `clients`, depending on the selected backends.

## Snapshots

Add `https://central.sonatype.com/repository/maven-snapshots` as a repository, then launch the snapshot explicitly:

```shell
cs launch --scala-version 2.13.16 \
  -r https://central.sonatype.com/repository/maven-snapshots \
  ba.sake::openapi4s-cli:0.9.1-SNAPSHOT \
  -M ba.sake.openapi4s.cli.OpenApi4sMain -- --help
```

Use the snapshot version reported by the main-branch CI job. Tagged releases are published to Maven Central.
