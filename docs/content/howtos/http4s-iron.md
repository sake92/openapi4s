---
layout: howto.html
title: Generate an http4s API with Iron
description: Generate constrained Scala types and expose the routes from an application
---

# {{ page.title }}

Use Circe models with the http4s server backend and Iron validation:

```shell
cs launch --scala-version 2.13.16 ba.sake::openapi4s-cli:0.9.0 \
  -M ba.sake.openapi4s.cli.OpenApi4sMain -- \
  --models circe \
  --framework http4s \
  --validation iron \
  --url openapi.yaml \
  --baseFolder src/main/scala \
  --basePackage com.example.api
```

Add the generated-code dependencies to your application:

```scala
//> using scala 3.9.0
//> using dep org.http4s::http4s-ember-server:0.23.38
//> using dep org.http4s::http4s-dsl:0.23.38
//> using dep org.http4s::http4s-circe:0.23.38
//> using dep io.circe::circe-generic:0.14.17
//> using dep io.github.iltotore::iron:3.4.1
//> using dep io.github.iltotore::iron-circe:3.4.1
```

Implement the `???` bodies in the generated routes, then mount them in your server:

```scala
import cats.effect.{IO, IOApp}
import com.comcast.ip4s.*
import com.example.api.routes.UsersRoutes
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.http4s.server.middleware.ErrorHandling

object Main extends IOApp.Simple:
  val run: IO[Unit] =
    val routes = ErrorHandling.httpRoutes[IO](new UsersRoutes().routes)
    EmberServerBuilder
      .default[IO]
      .withHost(host"127.0.0.1")
      .withPort(port"8080")
      .withHttpApp(routes.orNotFound)
      .build
      .useForever
```

Iron constraints are enforced while Circe decodes the request body. Invalid JSON is a `400 Bad Request`; structurally valid JSON that violates an Iron constraint is a `422 Unprocessable Entity`. The generated error handling supplies those client-facing responses.

Generated query matchers follow http4s route semantics: a missing required or malformed query parameter does not match the route and normally results in `404 Not Found`.
