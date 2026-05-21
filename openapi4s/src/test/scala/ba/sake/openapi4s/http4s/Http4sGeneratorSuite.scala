package ba.sake.openapi4s.http4s

import java.nio.file.Paths
import ba.sake.openapi4s._

class Http4sGeneratorSuite extends munit.FunSuite {

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
    println("*" * 100)
    println("Generated Http4s sources for petstore_3.0.0.json :")
    sources.foreach { source =>
      println("*" * 50)
      print("*" * 20)
      print(source.file.getFileName)
      println("*" * 20)
      println(source.source.syntax)
    }
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
    println("*" * 100)
    println("Generated Http4s sources for oneOf.yaml :")
    sources.foreach { source =>
      println("*" * 50)
      print("*" * 20)
      print(source.file.getFileName)
      println("*" * 20)
      println(source.source.syntax)
    }
  }
}
