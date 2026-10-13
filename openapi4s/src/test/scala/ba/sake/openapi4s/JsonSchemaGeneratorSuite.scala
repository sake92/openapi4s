package ba.sake.openapi4s

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

/** Regressions for JSON Schema constructs used by dependency analysis artifacts. */
class JsonSchemaGeneratorSuite extends munit.FunSuite {
  private def schemas(scope: String = "packages"): Path = {
    val folder = Files.createTempDirectory("codeps-json-schema-poc")
    Files.writeString(
      folder.resolve("definitions.json"),
      """{
      "title":"Definitions", "$defs": {
        "ScopeReport": {"type":"object", "required":["scope","cycles"], "properties":{
          "scope":{"type":"string","enum":["packages","files"]},
          "cycles":{"type":"array","items":{"type":"string"}},
          "truncation":{"type":"object","required":["omitted"],"properties":{"omitted":{"type":"integer"}}}
        }},
        "Edge":{"type":"object","required":["source","target"],"properties":{
          "source":{"type":"string"},"target":{"type":"string"},"weight":{"type":"integer","default":1}
        }}
      }
    }"""
    )
    val report = """{
      "title":"Report","type":"object","required":["packages"],"properties":{
        "packages":{"allOf":[{"$ref":"definitions.json#/$defs/ScopeReport"},
          {"properties":{"scope":{"const":"SCOPE"}}}]},
        "files":{"anyOf":[{"$ref":"definitions.json#/$defs/ScopeReport"},{"type":"null"}]},
        "edge":{"$ref":"definitions.json#/$defs/Edge"}
      }
    }""".replace("SCOPE", scope)
    Files.writeString(folder.resolve("report.json"), report)
    folder
  }

  private def generate(folder: Path): String = {
    val target = Files.createTempDirectory("codeps-json-schema-generated")
    OpenApiWriter(
      OpenApiWriter.Config(
        url = folder.toString,
        baseFolder = target,
        basePackage = "poc.models",
        models = "tupson",
        framework = "none"
      )
    ).write()
    val files = Files.walk(target)
    try files.iterator.asScala.filter(_.toString.endsWith(".scala")).map(Files.readString(_)).mkString("\n")
    finally files.close()
  }

  test("inline object allOf retains required field and narrows scope to const") {
    val generated = generate(schemas())
    assert(generated.contains("packages: Report__packages"), generated)
    assert(generated.contains("scope: \"packages\""), generated)
    assert(generated.contains("truncation: Option["), generated)
    assert(!generated.contains("Option[Option["), generated)
  }

  test("nullable referenced object stays typed instead of generic JSON") {
    val generated = generate(schemas())
    assert(generated.contains("files: Option[ScopeReport]"), generated)
    assert(!generated.contains("Option[JValue]"), generated)
  }

  test("optional integer schema default becomes a constructor default") {
    val generated = generate(schemas())
    assert(generated.contains("weight: Option[Long] = Some(1L)"), generated)
  }

  test("decimal integer defaults generate exact constructor literals") {
    val folder = schemas()
    val path = folder.resolve("definitions.json")
    Files.writeString(path, Files.readString(path).replace("\"default\":1", "\"default\":1.0"))
    assert(generate(folder).contains("weight: Option[Long] = Some(1L)"))
  }

  test("invalid integer defaults fail generation instead of dropping the field") {
    val folder = schemas()
    val path = folder.resolve("definitions.json")
    Files.writeString(path, Files.readString(path).replace("\"default\":1", "\"default\":1.5"))
    intercept[exceptions.UnsupportedSchemaException] { generate(folder) }
  }

  test("contradictory const intersection fails instead of dropping or widening a field") {
    intercept[exceptions.UnsupportedSchemaException] {
      generate(schemas("symbols"))
    }
  }

  test("unresolved and recursive allOf definitions retain the existing generation path") {
    val recursive = SchemaDefinition.AllOf(List(SchemaDefinition.Ref("Recursive")))
    val unknown = SchemaDefinition.AllOf(List(SchemaDefinition.Unknown()))
    val definition = OpenApiDefinition(
      NamedSchemaDefinitions(
        Seq(
          SchemaDefinition.Named("Recursive", recursive),
          SchemaDefinition.Named("UnknownIntersection", unknown)
        )
      ),
      PathDefinitions(List.empty)
    )
    val config = OpenApiWriter.Config(
      url = "unused",
      baseFolder = Files.createTempDirectory("json-schema-legacy"),
      basePackage = "poc.models",
      models = "tupson",
      framework = "none"
    )
    new tupson.TupsonModelGenerator(config, definition).generate()
  }
}
