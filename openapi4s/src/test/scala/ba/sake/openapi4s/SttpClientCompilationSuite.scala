package ba.sake.openapi4s

import java.nio.file.{Files, Path}
import scala.concurrent.duration.Duration

/** Compiles generated sttp client4 sources with scala-cli (Scala 3 compiler) against real sttp client4 jars, for both
  * the circe and tupson model backends. Also exercises the --tags filter.
  */
class SttpClientCompilationSuite extends munit.FunSuite {

  // first run resolves deps + downloads the Scala 3 compiler, so the default 30s timeout is not enough
  override def munitTimeout: Duration = Duration(10, "min")

  test("generated circe sttp client sources compile") {
    val baseFolder = generate("sttp_client.yaml", "com.example.circeclient", models = "circe", tags = None)
    CompilationTestUtils.compileGenerated(
      baseFolder.resolve("com/example/circeclient"),
      scalaVersion = "3.9.0",
      dependencies = Seq(
        "com.softwaremill.sttp.client4::core:4.1.0",
        "com.softwaremill.sttp.client4::circe:4.1.0",
        "io.circe::circe-core:0.14.17",
        "io.circe::circe-generic:0.14.17"
      )
    )
  }

  test("generated tupson sttp client sources compile (with --tags filter)") {
    val baseFolder =
      generate("sttp_client.yaml", "com.example.tupsonclient", models = "tupson", tags = Some(List("Pet")))
    CompilationTestUtils.compileGenerated(
      baseFolder.resolve("com/example/tupsonclient"),
      scalaVersion = "3.9.0",
      dependencies = Seq(
        "com.softwaremill.sttp.client4::core:4.1.0",
        "ba.sake::tupson:0.31.0",
        "ba.sake::tupson-sttp:0.31.0",
        "ba.sake::validson:0.20.0"
      )
    )
  }

  private def generate(url: String, basePackage: String, models: String, tags: Option[List[String]]): Path = {
    val baseFolder = Files.createTempDirectory(s"openapi4s-compile-sttp-${models}")
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl(url),
      baseFolder = baseFolder,
      basePackage = basePackage,
      models = models,
      framework = "none",
      client = "sttp",
      tags = tags
    )
    OpenApiWriter(config).write()
    baseFolder
  }
}
