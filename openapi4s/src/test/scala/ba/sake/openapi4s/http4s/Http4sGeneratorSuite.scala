package ba.sake.openapi4s.http4s

import ba.sake.openapi4s._

import java.nio.file.Paths

class Http4sGeneratorSuite extends munit.FunSuite {

  test("generateSources should generate from petstore_3.0.0.json") {
    val config = OpenApiGenerator.Config(
      url = TestUtils.getResourceUrl("petstore_3.0.0.json"),
      baseFolder = Paths.get("app"),
      basePackage = "pkg"
    )
    val openapiDefinition = OpenApiDefinition.parse(config.url)
    val generator = new Http4sGenerator(config, openapiDefinition)
    val sources = generator.generateSources
    println("*" * 100)
    println("Generated Sharaf sources for petstore_3.0.0.json :")
    sources.foreach { source =>
      println("*" * 50)
      print("*" * 20)
      print(source.file.getFileName)
      println("*" * 20)
      println(source.source.syntax)
    }
  }

  test("generateSources should generate from oneOf.yaml") {
    val config = OpenApiGenerator.Config(
      url = TestUtils.getResourceUrl("oneOf.yaml"),
      baseFolder = Paths.get("app"),
      basePackage = "pkg"
    )
    val openapiDefinition = OpenApiDefinition.parse(config.url)
    val generator = new Http4sGenerator(config, openapiDefinition)
    val sources = generator.generateSources
    println("*" * 100)
    println("Generated Sharaf sources for oneOf.yaml :")
    sources.foreach { source =>
      println("*" * 50)
      print("*" * 20)
      print(source.file.getFileName)
      println("*" * 20)
      println(source.source.syntax)
    }
  }
}
