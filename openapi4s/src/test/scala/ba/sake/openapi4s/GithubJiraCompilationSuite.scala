package ba.sake.openapi4s

import java.nio.file.{Files, Path}
import scala.concurrent.duration.Duration

/** Compiles generated tupson sources with scala-cli (Scala 3 compiler) to make sure they are valid. */
class GithubJiraCompilationSuite extends munit.FunSuite {

  // dotc compiling the ~800 github models takes a while, so the default 30s timeout is not enough
  override def munitTimeout: Duration = Duration(60, "min")

  test("generated tupson sources compile for github.json") {
    val baseFolder = generate("github.json", "com.example.github")
    CompilationTestUtils.compileGenerated(
      baseFolder.resolve("com/example/github"),
      scalaVersion = "3.7.3",
      dependencies = Seq("ba.sake::tupson:0.20.0", "ba.sake::validson:0.19.0")
    )
  }

  test("generated tupson sources compile for jira-cloud-v3.json") {
    val baseFolder = generate("jira-cloud-v3.json", "com.example.jira")
    CompilationTestUtils.compileGenerated(
      baseFolder.resolve("com/example/jira"),
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
