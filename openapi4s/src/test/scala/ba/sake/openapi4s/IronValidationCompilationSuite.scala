package ba.sake.openapi4s

import java.nio.file.{Files, Path}
import scala.concurrent.duration.Duration

/** Compiles generated circe sources with scala-cli (Scala 3 compiler) to make sure they are valid. */
class IronValidationCompilationSuite extends munit.FunSuite {

  // first CI run resolves deps + downloads the Scala 3 compiler, so the default 30s timeout is not enough
  override def munitTimeout: Duration = Duration(10, "min")

  test("generated circe + iron sources compile") {
    val baseFolder = generate(
      url = "iron_constraints.json",
      basePackage = "com.example.iron",
      validation = "iron"
    )
    CompilationTestUtils.compileGenerated(
      baseFolder.resolve("com/example/iron"),
      scalaVersion = "3.7.3",
      dependencies = Seq(
        "io.circe::circe-core:0.14.10",
        "io.circe::circe-generic:0.14.10",
        "io.github.iltotore::iron:3.0.2",
        "io.github.iltotore::iron-circe:3.0.2"
      )
    )
  }

  test("generated circe sources compile") {
    val baseFolder = generate(
      url = "petstore_3.0.0.json",
      basePackage = "com.example.plain",
      validation = "none"
    )
    CompilationTestUtils.compileGenerated(
      baseFolder.resolve("com/example/plain"),
      scalaVersion = "3.7.3",
      dependencies = Seq(
        "io.circe::circe-core:0.14.10",
        "io.circe::circe-generic:0.14.10"
      )
    )
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
