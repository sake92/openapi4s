---
title: Backends and dependencies
description: Compatible model, server, validation and client backends
---

# {{ page.title }}

Choose one model backend, an optional server backend, an optional validation backend and an optional client backend.

| Purpose | Backend | Compatibility |
|---|---|---|
| Models | `circe` | http4s, sttp and Iron |
| Models | `tupson` | Sharaf, sttp and Validson |
| Server | `http4s` | Use with Circe models |
| Server | `sharaf` | Use with Tupson models |
| Validation | `iron` | Use with Circe models |
| Validation | `validson` | Use with Tupson models |
| Client | `sttp` | Use with Circe or Tupson models |

Generated-code compilation is tested on Scala 3.9.0.

| Generated code | Dependencies |
|---|---|
| Tupson models | `ba.sake::tupson:0.31.0`, `ba.sake::validson:0.20.0` |
| Circe models | `io.circe::circe-core:0.14.17`, `io.circe::circe-generic:0.14.17` |
| Sharaf controllers | `ba.sake::sharaf:0.9.3` |
| http4s routes | `org.http4s::http4s-dsl:0.23.38`, plus `http4s-circe` for Circe models |
| Iron validation | `io.github.iltotore::iron:3.4.1`, `iron-circe:3.4.1` |
| sttp clients | `com.softwaremill.sttp.client4::core:4.1.0`, plus the selected JSON integration |

The [Mill plugin](https://github.com/sake92/mill-openapi4s) and [sbt plugin](https://github.com/sake92/sbt-openapi4s) expose equivalent build settings.
