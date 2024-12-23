package ba.sake.openapi4s.sharaf

import scala.jdk.CollectionConverters._
import io.swagger.parser.OpenAPIParser
import ba.sake.openapi4s.SchemaDefinition._
import java.nio.file.Paths
import ba.sake.openapi4s._

class SharafGeneratorSuite extends munit.FunSuite {

  test("generateSources should generate models and controllers") {
    val generator = new SharafGenerator()
    val config = OpenApiGenerator.Config(url = TestUtils.getResourceUrl( "petstore_3.0.0.json"), baseFolder = Paths.get("app"), basePackage = "pkg")
    val openapiDefinition = OpenApiDefinition.parse(config.url)
    val sources = generator.generateSources(config, openapiDefinition)
    /*sources.foreach { source =>
      println("*" * 50)
      print("*" * 20)
      print(source.file.getFileName)
      println("*" * 20)
      println(source.source.syntax)
    }*/
  }
}
