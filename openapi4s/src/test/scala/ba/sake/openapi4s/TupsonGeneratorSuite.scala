package ba.sake.openapi4s

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

class TupsonGeneratorSuite extends munit.FunSuite {

  test("tupson generator should generate modern schema constructs") {
    val baseFolder = Files.createTempDirectory("openapi4s-tupson-features")
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl("tupson_features.yaml"),
      baseFolder = baseFolder,
      basePackage = "pkg",
      models = "tupson",
      framework = "none"
    )
    OpenApiWriter(config).write()

    val containerFile = readGeneratedFile(baseFolder, "models/Container.scala")
    assert(
      containerFile.contains("meta: (kind: String, age: Int)"),
      s"anonymous object not a named tuple:\n$containerFile"
    )
    assert(
      containerFile.contains("nested: (inner: (x: String))"),
      s"nested anonymous object not a named tuple:\n$containerFile"
    )
    assert(containerFile.contains("items: Seq[(a: Int, b: String)]"), s"anonymous object in array:\n$containerFile")

    val withEnumFile = readGeneratedFile(baseFolder, "models/WithEnum.scala")
    assert(
      withEnumFile.contains("status: \"available\" | \"pending\" | \"sold\""),
      s"inline enum not a literal union:\n$withEnumFile"
    )
    assert(withEnumFile.contains("singleVal: \"only\""), s"single-value enum not a literal:\n$withEnumFile")

    val withIntEnumFile = readGeneratedFile(baseFolder, "models/WithIntEnum.scala")
    assert(withIntEnumFile.contains("num: 1 | 2"), s"integer enum not a literal union:\n$withIntEnumFile")

    val withMapFile = readGeneratedFile(baseFolder, "models/WithMap.scala")
    assert(withMapFile.contains("extra: Map[String, String]"), s"additionalProperties not a map:\n$withMapFile")
    assert(
      withMapFile.contains("freeForm: Map[String, JValue]"),
      s"free-form map not Map[String, JValue]:\n$withMapFile"
    )

    // oneOf without discriminator -> type alias union; with discriminator -> sealed trait
    val animalFile = readGeneratedFile(baseFolder, "models/Animal.scala")
    assert(animalFile.contains("type Animal = Cat | Dog"), s"discriminator-less oneOf not a union alias:\n$animalFile")

    val petFile = readGeneratedFile(baseFolder, "models/Pet.scala")
    assert(petFile.contains("@discriminator(\"pet_type\")"), s"discriminated oneOf lost discriminator:\n$petFile")

    // anyOf -> union alias
    val anyThingFile = readGeneratedFile(baseFolder, "models/AnyThing.scala")
    assert(anyThingFile.contains("type AnyThing = Cat | Dog"), s"anyOf not a union alias:\n$anyThingFile")

    // named non-string enum and named map -> type aliases
    val levelFile = readGeneratedFile(baseFolder, "models/Level.scala")
    assert(levelFile.contains("type Level = 1 | 2 | 3"), s"named integer enum not a literal union alias:\n$levelFile")

    val tagsFile = readGeneratedFile(baseFolder, "models/Tags.scala")
    assert(tagsFile.contains("type Tags = Map[String, String]"), s"named map not a type alias:\n$tagsFile")
  }

  test("tupson generator should generate const literals (3.1)") {
    val baseFolder = Files.createTempDirectory("openapi4s-tupson-31")
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl("tupson_features_31.yaml"),
      baseFolder = baseFolder,
      basePackage = "pkg",
      models = "tupson",
      framework = "none"
    )
    OpenApiWriter(config).write()

    val withConstFile = readGeneratedFile(baseFolder, "models/WithConst.scala")
    assert(withConstFile.contains("kind: \"dog\""), s"string const not a literal type:\n$withConstFile")
    assert(withConstFile.contains("count: 5"), s"integer const not a literal type:\n$withConstFile")

    val markerFile = readGeneratedFile(baseFolder, "models/Marker.scala")
    assert(markerFile.contains("type Marker = \"abc\""), s"named const not a type alias:\n$markerFile")

    val deviceFile = readGeneratedFile(baseFolder, "models/Device.scala")
    assert(
      deviceFile.contains("devicePlatform: Option[\"web\" | \"mob\"]"),
      s"optional nullable enum should have one Option level:\n$deviceFile"
    )
  }

  private def readGeneratedFile(base: Path, relative: String): String = {
    val file = base.resolve("pkg").resolve(relative)
    Files.readString(file)
  }
}
