package ba.sake.openapi4s

import java.nio.file.{Files, Path}
import scala.concurrent.duration.Duration

/** Compiles the tupson models generated from the full Jira Cloud v3 spec with scala-cli. */
class JiraCompilationSuite extends munit.FunSuite {

  // compiling ~700 derivation-heavy models takes ~4 minutes
  override def munitTimeout: Duration = Duration(30, "min")

  // ignored by default; remove `.ignore` and run locally (needs ~4 min and ~6g heap for the compiler)
  test("generated tupson sources compile for jira-cloud-v3.json".ignore) {
    val baseFolder = generate("jira-cloud-v3.json", "com.example.jira")
    CompilationTestUtils.compileGenerated(
      baseFolder.resolve("com/example/jira"),
      scalaVersion = "3.7.3",
      dependencies = Seq("ba.sake::tupson:0.30.0", "ba.sake::validson:0.19.0")
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
    baseFolder
  }
}
