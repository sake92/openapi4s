# openapi4s

OpenApi generators for Scala.

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


---

## Plugins

### Mill plugin

```scala
package build

import $ivy.`ba.sake::mill-openapi4s::0.0.9`
import mill._
import mill.scalalib._
import ba.sake.openapi4s.OpenApiGeneratorModule

object api extends ScalaModule with OpenApiGeneratorModule {
  def scalaVersion = "3.6.2"
  def ivyDeps = Agg(
    ivy"ba.sake::sharaf:0.8.0"
  )
  // openApi4s mandatory config
  def openApi4sPackage = "com.example.api"
  // openApi4s optional config
  //def openApi4sUrl: T[String] = T((millSourcePath / "resources" / "openapi.json").wrapped.toUri.toString)
  //def openApi4sTargetDir: T[os.Path] = T(millSourcePath / "src")
  //def openApi4sGenerator: T[String] = "sharaf"
}
```

```shell
./mill -i api.openApi4sGenerate
./mill -i api.compile
```

