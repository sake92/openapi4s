package ba.sake.openapi4s

import java.nio.file.{Files, Path}
import scala.concurrent.duration.Duration

/** Compiles the tupson models generated from the full GitHub REST API spec with scala-cli. */
class GithubCompilationSuite extends munit.FunSuite {

  // compiling ~800 derivation-heavy models takes ~12 minutes
  override def munitTimeout: Duration = Duration(60, "min")

  // ignored by default; remove `.ignore` and run locally (needs ~12 min and ~6g heap for the compiler)
  test("generated tupson sources compile for github.json".ignore) {
    val baseFolder = generate("github.json", "com.example.github")
    CompilationTestUtils.compileGenerated(
      baseFolder.resolve("com/example/github"),
      scalaVersion = "3.7.3",
      dependencies = Seq("ba.sake::tupson:0.20.0", "ba.sake::validson:0.19.0")
    )
  }

  private def generate(url: String, basePackage: String): Path = {
    val baseFolder = Files.createTempDirectory("openapi4s-compile-specs")
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl(url),
      baseFolder = baseFolder,
      basePackage = basePackage,
      models = "tupson",
      framework = "none"
    )
    OpenApiWriter(config).write()
    // tupson has no JsonRW[LocalDate]; provide it in the test (see CompilationTestUtils)
    CompilationTestUtils.writeLocalDateJsonRW(
      baseFolder.resolve(basePackage.replace('.', '/')).resolve("models"),
      basePackage
    )
    baseFolder
  }
}
