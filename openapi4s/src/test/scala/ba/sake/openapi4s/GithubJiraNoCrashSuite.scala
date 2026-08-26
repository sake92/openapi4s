package ba.sake.openapi4s

import java.nio.file.Files

class GithubJiraNoCrashSuite extends munit.FunSuite {

  test("tupson+sharaf generation completes for github.json") {
    run("github.json", "com.example.github")
  }

  test("tupson+sharaf generation completes for jira-cloud-v3.json") {
    run("jira-cloud-v3.json", "com.example.jira")
  }

  test("circe model generation completes for github.json") {
    val baseFolder = Files.createTempDirectory("openapi4s-nocrash")
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl("github.json"),
      baseFolder = baseFolder,
      basePackage = "com.example.github",
      models = "circe",
      framework = "none"
    )
    val sources = OpenApiWriter(config).write()
    assert(sources.nonEmpty, s"No sources generated for github.json")
  }

  private def run(resource: String, basePackage: String): Unit = {
    val baseFolder = Files.createTempDirectory("openapi4s-nocrash")
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl(resource),
      baseFolder = baseFolder,
      basePackage = basePackage,
      models = "tupson",
      framework = "sharaf"
    )
    val sources = OpenApiWriter(config).write()
    assert(sources.nonEmpty, s"No sources generated for ${resource}")
  }
}
