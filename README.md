# openapi4s

OpenApi generators for Scala.

Here is a small video demo: https://youtu.be/kf0vGrlKNb8

## Features and Benefits
- incremental generator
  - doesn't touch the code that you added manually
  - only adds new properties/methods/classes
- lenient parser+generator
  - if something is not supported it will still (mostly) work
  - you can adapt your openapi spec to work gradually

## Limitations
- JSON only
- only *named* entities, no anonymous objects
- no comments in source code (scalameta limitation)
- scalafmt is almost mandatory (if you care about nice/minimal git diffs)

---

## Generators

### Sharaf backend
Supports almost all features:
- controllers
- discriminated models (sealed traits)
- enums (scala3 singleton enums)
- validations
- query params

### Http4s backend
Supports some features:
- routes (controllers)
- discriminated models (sealed traits)
- enums (scala3 singleton enums)

TODO: query params, validation..  
Contributions welcome!

---

## Plugins

### Mill plugin

```scala
package build

import $ivy.`ba.sake::mill-openapi4s::0.2.0`
import mill._
import mill.scalalib._, scalafmt._
import ba.sake.openapi4s.OpenApiGeneratorModule

object api extends ScalaModule with OpenApiGeneratorModule with ScalafmtModule {
  def scalaVersion = "3.6.2"
  def ivyDeps = Agg(
    // sharaf
    ivy"ba.sake::sharaf:0.8.0"
    // http4s
    ivy"org.http4s::http4s-ember-server:0.23.29",
    ivy"org.http4s::http4s-circe:0.23.29",
    ivy"org.http4s::http4s-dsl:0.23.29"
  )
  /* mandatory config */
  def openApi4sPackage = "com.example.api"
  
  /* optional config */
  //def openApi4sGenerator: T[String] = "sharaf" // or "http4s"
  //def openApi4sFile = T.source(PathRef(millSourcePath / "resources" / "openapi.json"))
  //def openApi4sTargetDir: T[PathRef] = T(millSourcePath / "src")
  
}
```

```shell
# preferrably first do a scalafmt for better git-diff
./mill api.reformat
./mill api.compile
```

