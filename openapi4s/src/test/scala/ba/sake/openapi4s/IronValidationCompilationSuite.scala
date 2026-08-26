package ba.sake.openapi4s

import java.nio.file.{Files, Path}

/** Compiles generated Scala 3 sources with the Scala 3 compiler to make sure they are valid. */
class IronValidationCompilationSuite extends munit.FunSuite {

  // dotc compilation is slow, especially when run in parallel with other test classes
  override def munitTimeout: scala.concurrent.duration.Duration =
    scala.concurrent.duration.Duration(120, "s")

  test("generated circe + iron sources compile") {
    val baseFolder = generate(
      url = "iron_constraints.json",
      basePackage = "com.example.iron",
      validation = "iron"
    )
    CompilationTestUtils.compileGenerated(baseFolder.resolve("com/example/iron"))
  }

  test("generated circe sources compile") {
    val baseFolder = generate(
      url = "petstore_3.0.0.json",
      basePackage = "com.example.plain",
      validation = "none"
    )
    CompilationTestUtils.compileGenerated(baseFolder.resolve("com/example/plain"))
  }

  private def generate(url: String, basePackage: String, validation: String): Path = {
    val baseFolder = Files.createTempDirectory(s"openapi4s-compile-${validation}")
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl(url),
      baseFolder = baseFolder,
      basePackage = basePackage,
      models = "circe",
      framework = "none",
      validation = validation
    )
    OpenApiWriter(config).write()
    baseFolder
  }
}
