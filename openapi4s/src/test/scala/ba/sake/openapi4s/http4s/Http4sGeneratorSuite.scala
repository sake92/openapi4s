package ba.sake.openapi4s.http4s

import java.nio.file.{Files, Paths, StandardCopyOption}
import scala.concurrent.duration._
import ba.sake.openapi4s._

class Http4sGeneratorSuite extends munit.FunSuite {

  override def munitTimeout = 10.minutes

  test("generated query parameter routes compile and decode requests with http4s") {
    val baseFolder = Files.createTempDirectory("openapi4s-http4s-query")
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl("http4s_query.yaml"),
      baseFolder = baseFolder,
      basePackage = "querytest",
      models = "circe",
      framework = "http4s"
    )
    OpenApiWriter(config).write()
    // Fill in the generated scaffold as a consumer would, so we also verify the bound values.
    val routesFile = baseFolder.resolve("querytest/routes/QueryRoutes.scala")
    val routesSource = Files.readString(routesFile)
    val response = """Ok(List(limit.toString, queryParam1.toString, queryParam2.toString,
      ids.mkString(","), colors.mkString(","), status.toString, id.toString, date.toString,
      at.toString, ratio.toString, score.toString, state.toString).mkString("|"))"""
    val placeholder = "NotImplemented(\"TODO: return String\")"
    assert(routesSource.contains(placeholder))
    Files.writeString(routesFile, routesSource.replace(placeholder, response))
    val check = getClass.getResourceAsStream("/http4s_query_check.scala")
    try Files.copy(check, baseFolder.resolve("QueryCheck.scala"), StandardCopyOption.REPLACE_EXISTING)
    finally check.close()
    CompilationTestUtils.runGenerated(
      baseFolder,
      scalaVersion = "3.7.3",
      dependencies = Seq(
        "org.http4s::http4s-dsl:0.23.35",
        "org.http4s::http4s-circe:0.23.35",
        "io.circe::circe-generic:0.14.10"
      ),
      mainClass = "QueryCheck"
    )
  }

  test("generate() should generate from petstore_3.0.0.json") {
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl("petstore_3.0.0.json"),
      baseFolder = Paths.get("app"),
      basePackage = "mypkg",
      models = "circe",
      framework = "http4s"
    )
    val openapiDefinition = OpenApiDefinition.parse(config.url)
    val modelImportContract = ModelImportContracts.circe
    val generator = new Http4sGenerator(
      config,
      openapiDefinition,
      modelImportContract.frameworkImportsById(FrameworkBackendId.Http4s)
    )
    val sources = generator.generate()
    TestUtils.printGeneratedSources("Http4s sources for petstore_3.0.0.json", sources)
  }

  test("generate() should generate from oneOf.yaml") {
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl("oneOf.yaml"),
      baseFolder = Paths.get("app"),
      basePackage = "mypkg",
      models = "circe",
      framework = "http4s"
    )
    val openapiDefinition = OpenApiDefinition.parse(config.url)
    val modelImportContract = ModelImportContracts.circe
    val generator = new Http4sGenerator(
      config,
      openapiDefinition,
      modelImportContract.frameworkImportsById(FrameworkBackendId.Http4s)
    )
    val sources = generator.generate()
    TestUtils.printGeneratedSources("Http4s sources for oneOf.yaml", sources)
  }
}
