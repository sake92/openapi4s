package ba.sake.openapi4s

import java.nio.file.{Files, Path}
import scala.concurrent.duration._

class DiscriminatorMappingSuite extends munit.FunSuite {
  override def munitTimeout = 10.minutes

  test("parsing and normalization preserve explicit discriminator mappings") {
    val definition = OpenApiDefinition.parse(TestUtils.getResourceUrl("discriminator_mapping.json"))
    val pet = definition.namedSchemaDefinitions.defs.find(_.name == "Pet").get.schema
    val expected = SchemaDefinition.OneOf(
      List(SchemaDefinition.Ref("Cat"), SchemaDefinition.Ref("Dog")),
      Some("kind"),
      Map("cat" -> "#/components/schemas/Cat", "dog" -> "#/components/schemas/Dog")
    )
    assertEquals(pet, expected)
    assertEquals(SchemaNormalizer.normalize(pet, Map.empty, "Pet"), expected)
  }

  test("generated Circe + Iron http4s codecs honor discriminator mappings across regeneration") {
    val folder = Files.createTempDirectory("openapi4s-discriminator-mapping")
    val original = resource("discriminator_mapping.json")
    generate(folder, original)
    val routes = folder.resolve("example/routes/PetsRoutes.scala")
    Files.writeString(
      routes,
      Files.readString(routes).replace("NotImplemented(\"TODO: return Pet\")", "Ok(reqBody)")
    )
    check(folder, resource("discriminator_mapping_check.scala"))

    generate(folder, original.replace("\"cat\"", "\"feline-v1\""))
    check(folder, resource("discriminator_mapping_check.scala").replace("\"cat\"", "\"feline-v1\""))
    val model = folder.resolve("example/models/Pet.scala")
    val updated = Files.readString(model)
    generate(folder, original.replace("\"cat\"", "\"feline-v1\""))
    assertEquals(Files.readString(model), updated)
    assert(Files.readString(routes).contains("Ok(reqBody)"))
  }

  test("partial mappings support schema-name targets and implicit names for unmapped cases") {
    val folder = Files.createTempDirectory("openapi4s-partial-discriminator-mapping")
    val spec = resource("discriminator_mapping.json")
      .replace(",\n            \"dog\": \"#/components/schemas/Dog\"", "")
      .replace("\"cat\": \"#/components/schemas/Cat\"", "\"cat\": \"Cat\"")
      .replace("\"dog\"", "\"Dog\"")
    generate(folder, spec)
    val routes = folder.resolve("example/routes/PetsRoutes.scala")
    Files.writeString(routes, Files.readString(routes).replace("NotImplemented(\"TODO: return Pet\")", "Ok(reqBody)"))
    check(folder, resource("discriminator_mapping_check.scala").replace("\"dog\"", "\"Dog\""))
  }

  test("multiple wire values for one constructor fail explicitly instead of silently choosing a mapping") {
    val folder = Files.createTempDirectory("openapi4s-ambiguous-discriminator-mapping")
    val spec = resource("discriminator_mapping.json")
      .replace("\"dog\": \"#/components/schemas/Dog\"", "\"kitten\": \"#/components/schemas/Cat\"")
    val error = intercept[exceptions.UnsupportedSchemaException](generate(folder, spec))
    assert(error.getMessage.contains("Multiple discriminator values for 'Cat'"))
  }

  private def generate(folder: Path, spec: String): Unit = {
    val input = folder.resolve("openapi.json")
    Files.writeString(input, spec)
    OpenApiWriter(
      OpenApiWriter.Config(input.toString, folder, "example", "circe", "http4s", validation = "iron")
    ).write()
    ()
  }

  private def check(folder: Path, source: String): Unit = {
    Files.writeString(folder.resolve("DiscriminatorMappingCheck.scala"), source)
    CompilationTestUtils.runGenerated(
      folder,
      "3.9.0",
      Seq(
        "org.http4s::http4s-dsl:0.23.38",
        "org.http4s::http4s-circe:0.23.38",
        "org.http4s::http4s-server:0.23.38",
        "io.circe::circe-generic:0.14.17",
        "io.github.iltotore::iron:3.4.1",
        "io.github.iltotore::iron-circe:3.4.1"
      ),
      "DiscriminatorMappingCheck"
    )
  }

  private def resource(name: String): String = {
    val stream = getClass.getResourceAsStream(s"/$name")
    try new String(stream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
    finally stream.close()
  }
}
