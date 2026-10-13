package ba.sake.openapi4s

import java.nio.file.{Files, Paths}

class JsonSchemaDefinitionSuite extends munit.FunSuite {

  test("explicit null-only schemas do not infer an object from properties") {
    for (useTypes <- List(false, true)) {
      val schema = new io.swagger.v3.oas.models.media.JsonSchema()
      if (useTypes) schema.addType("null") else schema.setType("null")
      schema.addProperty("x", new io.swagger.v3.oas.models.media.StringSchema())
      intercept[exceptions.UnsupportedSchemaDefinitionException] {
        new SchemaDefinitionResolver().resolveSchema(schema, "null-only")
      }
    }
  }

  test("properties without an explicit type still infer an object") {
    val schema = new io.swagger.v3.oas.models.media.JsonSchema()
    schema.addProperty("x", new io.swagger.v3.oas.models.media.StringSchema())
    assert(new SchemaDefinitionResolver().resolveSchema(schema, "property-only").isInstanceOf[SchemaDefinition.Obj])
  }

  test("parses a single JSON Schema file and promotes $defs to named schemas") {
    val definition = OpenApiDefinition.parse(TestUtils.getResourceUrl("json-schema/single/person.json"))

    assertEquals(definition.pathDefinitions.defs, List.empty)
    assertEquals(definition.namedSchemaDefinitions.defs.map(_.name).toSet, Set("Person", "Status"))
  }

  test("parses a JSON Schema directory and resolves references between files") {
    val folder = Paths.get(getClass.getClassLoader.getResource("json-schema/folder").toURI)
    val definition = OpenApiDefinition.parse(folder.toString)
    val customer = definition.namedSchemaDefinitions.defs.find(_.name == "Customer").getOrElse(fail("Customer missing"))

    assertEquals(definition.namedSchemaDefinitions.defs.map(_.name).toSet, Set("Address", "Customer"))
    assert(customer.schema.toString.contains("Ref(Address)"))
  }

  test("qualifies colliding definitions and preserves local and cross-file references") {
    val folder = Files.createTempDirectory("json-schema-names")
    Files.writeString(
      folder.resolve("first.json"),
      """{
      "title":"First", "type":"object", "required":["node","root","unique"], "properties":{
        "node":{"$ref":"#/$defs/fileNode"},
        "root":{"$ref":"#/$defs/second"},
        "unique":{"$ref":"#/$defs/First__fileNode"}
      }, "$defs":{
        "fileNode":{"type":"object","properties":{"local":{"$ref":"#/$defs/declarationSurface"}}},
        "second":{"type":"object"},
        "First__fileNode":{"type":"object"},
        "declarationSurface":{"type":"object"}
      }
    }"""
    )
    Files.writeString(
      folder.resolve("second.json"),
      """{
      "title":"Second", "type":"object", "required":["node","other"], "properties":{
        "node":{"$ref":"#/$defs/FileNode"},
        "other":{"$ref":"first.json#/$defs/fileNode"}
      }, "$defs":{"FileNode":{"type":"object"}}
    }"""
    )
    val definitions =
      JsonSchemaDefinition.parse(folder.toString).namedSchemaDefinitions.defs.map(d => d.name -> d.schema).toMap
    assertEquals(
      definitions.keySet,
      Set(
        "First",
        "Second",
        "First__fileNode",
        "Second__FileNode",
        "First__second",
        "First__First__fileNode",
        "DeclarationSurface"
      )
    )
    def reference(model: String, property: String): SchemaDefinition = definitions(model) match {
      case SchemaDefinition.Obj(properties) => properties.find(_.name == property).get.schema
      case other                            => fail(s"Expected object, got $other")
    }
    assertEquals(reference("First", "node"), SchemaDefinition.Ref("First__fileNode"))
    assertEquals(reference("First", "root"), SchemaDefinition.Ref("First__second"))
    assertEquals(reference("First", "unique"), SchemaDefinition.Ref("First__First__fileNode"))
    assertEquals(reference("Second", "node"), SchemaDefinition.Ref("Second__FileNode"))
    assertEquals(reference("Second", "other"), SchemaDefinition.Ref("First__fileNode"))
    assertEquals(
      reference("First__fileNode", "local"),
      SchemaDefinition.Opt(SchemaDefinition.Ref("DeclarationSurface"))
    )
  }

  test("rejects component collisions that remain after qualification") {
    val folder = Files.createTempDirectory("json-schema-name-conflict")
    Files.writeString(
      folder.resolve("first.json"),
      """{
      "title":"First", "$defs":{"node":{"type":"object"}}
    }"""
    )
    Files.writeString(folder.resolve("node.json"), """{"title":"Node","type":"object"}""")
    Files.writeString(folder.resolve("qualified.json"), """{"title":"First__node","type":"object"}""")
    interceptMessage[RuntimeException]("JSON Schema component names must be unique; duplicates: First__node.") {
      JsonSchemaDefinition.parse(folder.toString)
    }
  }

  test("parses Snowplow self-describing object schemas") {
    val definition = OpenApiDefinition.parse(TestUtils.getResourceUrl("json-schema/single/button_click.json"))
    val buttonClick =
      definition.namedSchemaDefinitions.defs.find(_.name == "button_click").getOrElse(fail("button_click missing"))

    buttonClick.schema match {
      case SchemaDefinition.Obj(properties) =>
        assertEquals(properties.map(_.name), List("label", "id", "classes", "name"))
        assert(properties.find(_.name == "id").exists(_.schema.isInstanceOf[SchemaDefinition.Opt]))
        assert(properties.find(_.name == "classes").exists(_.schema.isInstanceOf[SchemaDefinition.Opt]))
      case other => fail(s"Expected object schema, got $other")
    }
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
