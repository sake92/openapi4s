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
    val sources = OpenApiWriter(config).write()
    TestUtils.printGeneratedSources("Http4s query parameter scaffold", sources)
    // Fill in the generated scaffold as a consumer would, so we also verify the bound values.
    val routesFile = baseFolder.resolve("querytest/routes/QueryRoutes.scala")
    val routesSource = Files.readString(routesFile)
    val optionalLimitMatcher =
      "private object LimitQueryParamMatcher2 extends OptionalQueryParamDecoderMatcher[String](\"limit\")"
    assertEquals(routesSource.sliding(optionalLimitMatcher.length).count(_ == optionalLimitMatcher), 1)
    assert(!routesSource.contains("(using "))
    assert(!routesSource.contains("QueryParam0_"))
    val consumerDecoders = List(
      "UUID" -> """QueryParamDecoder.fromUnsafeCast[UUID](value => UUID.fromString(value.value))("UUID")""",
      "LocalDate" -> "QueryParamDecoder.localDate(java.time.format.DateTimeFormatter.ISO_LOCAL_DATE)",
      "Instant" -> "QueryParamDecoder.instant(java.time.format.DateTimeFormatter.ISO_INSTANT)",
      "State" -> """QueryParamDecoder.fromUnsafeCast[State](value => State.valueOf(value.value))("State")"""
    )
    val completedSource = consumerDecoders.foldLeft(routesSource) { case (code, (tpe, decoder)) =>
      val placeholder = s"given QueryParamDecoder[$tpe] = ???"
      assertEquals(routesSource.sliding(placeholder.length).count(_ == placeholder), 1)
      assert(routesSource.contains(s"// TODO: implement query parameter decoding\n  $placeholder"))
      code.replace(placeholder, s"given QueryParamDecoder[$tpe] = $decoder")
    }
    val response = """Ok(List(limit.toString, queryParam1.toString, queryParam2.toString,
      ids.mkString(","), colors.mkString(","), status.toString, id.toString, date.toString,
      at.toString, ratio.toString, score.toString, state.toString).mkString("|"))"""
    val placeholder = "NotImplemented(\"TODO: return String\")"
    assert(routesSource.contains(placeholder))
    val completedRoutes = completedSource
      .replace("class QueryRoutes", "class CompletedQueryRoutes")
      .replace(placeholder, response)
    // Compile the untouched scaffold too; run a copy with the TODOs filled in by the consumer.
    Files.writeString(routesFile.resolveSibling("CompletedQueryRoutes.scala"), completedRoutes)
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
