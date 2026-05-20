package ba.sake.openapi4s

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

class OpenApiGeneratorSuite extends munit.FunSuite {

  test("composed generator should support circe + http4s") {
    val baseFolder = Files.createTempDirectory("openapi4s-circe-http4s")
    val config = OpenApiGenerator.Config(
      url = TestUtils.getResourceUrl("petstore_3.0.0.json"),
      baseFolder = baseFolder,
      basePackage = "pkg",
      models = "circe",
      framework = "http4s"
    )
    OpenApiGenerator(config).generate()
    val generatedFiles = listScalaFiles(baseFolder.resolve("pkg"))
    assert(generatedFiles.nonEmpty)
    assert(generatedFiles.exists(_.startsWith("models/")))
    assert(generatedFiles.exists(_.startsWith("routes/")))
    assert(!generatedFiles.exists(_.startsWith("controllers/")))
  }

  test("composed generator should support tupson + none") {
    val baseFolder = Files.createTempDirectory("openapi4s-tupson-none")
    val config = OpenApiGenerator.Config(
      url = TestUtils.getResourceUrl("petstore_3.0.0.json"),
      baseFolder = baseFolder,
      basePackage = "pkg",
      models = "tupson",
      framework = "none"
    )
    OpenApiGenerator(config).generate()
    val generatedFiles = listScalaFiles(baseFolder.resolve("pkg"))
    assert(generatedFiles.nonEmpty)
    assert(generatedFiles.exists(_.startsWith("models/")))
    assert(!generatedFiles.exists(_.startsWith("routes/")))
    assert(!generatedFiles.exists(_.startsWith("controllers/")))
  }

  test("composed generator should support none + http4s") {
    val baseFolder = Files.createTempDirectory("openapi4s-none-http4s")
    val config = OpenApiGenerator.Config(
      url = TestUtils.getResourceUrl("petstore_3.0.0.json"),
      baseFolder = baseFolder,
      basePackage = "pkg",
      models = "none",
      framework = "http4s"
    )
    OpenApiGenerator(config).generate()
    val generatedFiles = listScalaFiles(baseFolder.resolve("pkg"))
    assert(generatedFiles.nonEmpty)
    assert(!generatedFiles.exists(_.startsWith("models/")))
    assert(generatedFiles.exists(_.startsWith("routes/")))
    assert(!generatedFiles.exists(_.startsWith("controllers/")))
  }

  test("composed generator should reject none + none") {
    interceptMessage[RuntimeException]("Invalid config: models=none and framework=none means nothing to generate.") {
      OpenApiGenerator(
        OpenApiGenerator.Config(
          url = TestUtils.getResourceUrl("petstore_3.0.0.json"),
          baseFolder = Paths.get("app"),
          basePackage = "pkg",
          models = "none",
          framework = "none"
        )
      )
    }
  }

  test("composed generator should allow circe + sharaf") {
    // This combination intentionally emits a compatibility warning to stderr, but generation should still succeed.
    val baseFolder = Files.createTempDirectory("openapi4s-circe-sharaf")
    val config = OpenApiGenerator.Config(
      url = TestUtils.getResourceUrl("petstore_3.0.0.json"),
      baseFolder = baseFolder,
      basePackage = "pkg",
      models = "circe",
      framework = "sharaf"
    )
    OpenApiGenerator(config).generate()
    val generatedFiles = listScalaFiles(baseFolder.resolve("pkg"))
    assert(generatedFiles.nonEmpty)
    assert(generatedFiles.exists(_.startsWith("models/")))
    assert(generatedFiles.exists(_.startsWith("controllers/")))
  }

  test("legacy generator mapping should still work") {
    val baseFolder = Files.createTempDirectory("openapi4s-legacy-http4s")
    val config = OpenApiGenerator.Config(
      url = TestUtils.getResourceUrl("petstore_3.0.0.json"),
      baseFolder = baseFolder,
      basePackage = "pkg"
    )
    OpenApiGenerator("http4s", config).generate()
    val generatedFiles = listScalaFiles(baseFolder.resolve("pkg"))
    assert(generatedFiles.nonEmpty)
    assert(generatedFiles.exists(_.startsWith("models/")))
    assert(generatedFiles.exists(_.startsWith("routes/")))
  }

  private def listScalaFiles(base: Path): List[String] = {
    if (!Files.exists(base)) List.empty
    else {
      val stream = Files.walk(base)
      try {
        stream.iterator().asScala
          .filter(path => Files.isRegularFile(path) && path.getFileName.toString.endsWith(".scala"))
          .map(path => base.relativize(path).toString.replace('\\', '/'))
          .toList
      } finally stream.close()
    }
  }
}
