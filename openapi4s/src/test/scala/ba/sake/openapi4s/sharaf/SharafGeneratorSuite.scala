package ba.sake.openapi4s.sharaf

import java.nio.file.Paths
import ba.sake.openapi4s._

class SharafGeneratorSuite extends munit.FunSuite {

  test("generate() should generate from petstore_3.0.0.json") {
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl("petstore_3.0.0.json"),
      baseFolder = Paths.get("app"),
      basePackage = "mypkg",
      models = "tupson",
      framework = "sharaf"
    )
    val openapiDefinition = OpenApiDefinition.parse(config.url)
    val modelImportContract = ModelImportContracts.tupson
    val generator = new SharafGenerator(
      config,
      openapiDefinition,
      modelImportContract.frameworkImportsById(FrameworkBackendId.Sharaf)
    )
    val sources = generator.generate()
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

  test("generate() should generate from oneOf.yaml") {
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl("oneOf.yaml"),
      baseFolder = Paths.get("app"),
      basePackage = "mypkg",
      models = "tupson",
      framework = "sharaf"
    )
    val openapiDefinition = OpenApiDefinition.parse(config.url)
    val modelImportContract = ModelImportContracts.tupson
    val generator = new SharafGenerator(
      config,
      openapiDefinition,
      modelImportContract.frameworkImportsById(FrameworkBackendId.Sharaf)
    )
    val sources = generator.generate()
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

  test("generate() should generate from spring_petclinic.yaml") {
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl("spring_petclinic.yaml"),
      baseFolder = Paths.get("app"),
      basePackage = "mypkg",
      models = "tupson",
      framework = "sharaf"
    )
    val openapiDefinition = OpenApiDefinition.parse(config.url)
    val modelImportContract = ModelImportContracts.tupson
    val generator = new SharafGenerator(
      config,
      openapiDefinition,
      modelImportContract.frameworkImportsById(FrameworkBackendId.Sharaf)
    )
    val sources = generator.generate()
    println("*" * 100)
    println("Generated Sharaf sources for spring_petclinic.yaml :")
    sources.foreach { source =>
      println("*" * 50)
      print("*" * 20)
      print(source.file.getFileName)
      println("*" * 20)
      println(source.source.syntax)
    }
  }
}
