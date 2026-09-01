package ba.sake.openapi4s

import java.nio.file.{Files, Paths}

class JsonSchemaDefinitionSuite extends munit.FunSuite {

  test("parses a single JSON Schema file and promotes $defs to named schemas") {
    val definition = OpenApiDefinition.parse(TestUtils.getResourceUrl("json-schema/single/person.json"))

    assertEquals(definition.pathDefinitions.defs, List.empty)
    assertEquals(definition.namedSchemaDefinitions.defs.map(_.name).toSet, Set("Person", "Person__Status"))
  }

  test("parses a JSON Schema directory and resolves references between files") {
    val folder = Paths.get(getClass.getClassLoader.getResource("json-schema/folder").toURI)
    val definition = OpenApiDefinition.parse(folder.toString)
    val customer = definition.namedSchemaDefinitions.defs.find(_.name == "Customer").getOrElse(fail("Customer missing"))

    assertEquals(definition.namedSchemaDefinitions.defs.map(_.name).toSet, Set("Address", "Customer"))
    assert(customer.schema.toString.contains("Ref(Address)"))
  }

  test("JSON Schema input rejects server and client generation") {
    val baseFolder = Files.createTempDirectory("openapi4s-json-schema")
    interceptMessage[RuntimeException](
      "JSON Schema input supports model generation only; omit --framework and --client."
    ) {
      OpenApiWriter(
        OpenApiWriter.Config(
          url = TestUtils.getResourceUrl("json-schema/single/person.json"),
          baseFolder = baseFolder,
          basePackage = "pkg",
          models = "tupson",
          framework = "sharaf"
        )
      )
    }
  }
}
