package ba.sake.openapi4s

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

class OpenApiGeneratorSuite extends munit.FunSuite {

  test("composed generator should support circe + http4s") {
    val baseFolder = Files.createTempDirectory("openapi4s-circe-http4s")
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl("petstore_3.0.0.json"),
      baseFolder = baseFolder,
      basePackage = "pkg",
      models = "circe",
      framework = "http4s"
    )
    OpenApiWriter(config).write()
    val generatedFiles = listScalaFiles(baseFolder.resolve("pkg"))
    assert(generatedFiles.nonEmpty)
    assert(generatedFiles.exists(_.startsWith("models/")))
    assert(generatedFiles.exists(_.startsWith("routes/")))
    assert(!generatedFiles.exists(_.startsWith("controllers/")))
    val routesFile = readGeneratedFile(baseFolder.resolve("pkg"), "routes/")
    assert(routesFile.contains("import org.http4s.circe.CirceEntityCodec.*"))
    printGeneratedSources(baseFolder.resolve("pkg"))
  }

  test("composed generator should support tupson + none") {
    val baseFolder = Files.createTempDirectory("openapi4s-tupson-none")
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl("petstore_3.0.0.json"),
      baseFolder = baseFolder,
      basePackage = "pkg",
      models = "tupson",
      framework = "none"
    )
    OpenApiWriter(config).write()
    val generatedFiles = listScalaFiles(baseFolder.resolve("pkg"))
    assert(generatedFiles.nonEmpty)
    assert(generatedFiles.exists(_.startsWith("models/")))
    assert(!generatedFiles.exists(_.startsWith("routes/")))
    assert(!generatedFiles.exists(_.startsWith("controllers/")))
    printGeneratedSources(baseFolder.resolve("pkg"))
  }

  test("composed generator should support none + http4s") {
    val baseFolder = Files.createTempDirectory("openapi4s-none-http4s")
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl("petstore_3.0.0.json"),
      baseFolder = baseFolder,
      basePackage = "pkg",
      models = "none",
      framework = "http4s"
    )
    OpenApiWriter(config).write()
    val generatedFiles = listScalaFiles(baseFolder.resolve("pkg"))
    assert(generatedFiles.nonEmpty)
    assert(!generatedFiles.exists(_.startsWith("models/")))
    assert(generatedFiles.exists(_.startsWith("routes/")))
    assert(!generatedFiles.exists(_.startsWith("controllers/")))
    val routesFile = readGeneratedFile(baseFolder.resolve("pkg"), "routes/")
    assert(!routesFile.contains("import org.http4s.circe.CirceEntityCodec.*"))
    printGeneratedSources(baseFolder.resolve("pkg"))
  }

  test("composed generator should reject none + none") {
    interceptMessage[RuntimeException](
      "Invalid config: models=none, framework=none and client=none means nothing to generate."
    ) {
      OpenApiWriter(
        OpenApiWriter.Config(
          url = TestUtils.getResourceUrl("petstore_3.0.0.json"),
          baseFolder = Paths.get("app"),
          basePackage = "pkg",
          models = "none",
          framework = "none"
        )
      )
    }
  }

  test("composed generator should allow circe + sharaf") {
    val baseFolder = Files.createTempDirectory("openapi4s-circe-sharaf")
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl("petstore_3.0.0.json"),
      baseFolder = baseFolder,
      basePackage = "pkg",
      models = "circe",
      framework = "sharaf"
    )
    OpenApiWriter(config).write()
    val generatedFiles = listScalaFiles(baseFolder.resolve("pkg"))
    assert(generatedFiles.nonEmpty)
    assert(generatedFiles.exists(_.startsWith("models/")))
    assert(generatedFiles.exists(_.startsWith("controllers/")))
    printGeneratedSources(baseFolder.resolve("pkg"))
  }

  test("composed generator should support circe + iron validation") {
    val baseFolder = Files.createTempDirectory("openapi4s-circe-iron")
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl("petstore_3.0.0.json"),
      baseFolder = baseFolder,
      basePackage = "pkg",
      models = "circe",
      framework = "none",
      validation = "iron"
    )
    OpenApiWriter(config).write()
    val generatedFiles = listScalaFiles(baseFolder.resolve("pkg"))
    assert(generatedFiles.nonEmpty)
    assert(
      generatedFiles.exists(_.startsWith("models/Newtypes.scala")),
      s"Expected models/Newtypes.scala in: ${generatedFiles.mkString(", ")}"
    )
    assert(
      generatedFiles.exists(gf => gf.startsWith("models/") && !gf.contains("Newtypes")),
      "Expected at least one model file besides Newtypes.scala"
    )

    val newtypesFile = readGeneratedFile(baseFolder.resolve("pkg"), "models/Newtypes.scala")
    assert(
      newtypesFile.contains("import io.github.iltotore.iron.*"),
      "Newtypes.scala should import io.github.iltotore.iron.*"
    )
    assert(
      newtypesFile.contains("import io.github.iltotore.iron.constraint.all.*"),
      "Newtypes.scala should import iron constraints"
    )
    assert(
      newtypesFile.contains("extends io.github.iltotore.iron.RefinedType"),
      "Newtypes.scala should contain RefinedType definitions"
    )

    val petFile = readGeneratedFile(baseFolder.resolve("pkg"), "models/Pet.scala")
    assert(petFile.contains("import io.github.iltotore.iron.circe.given"), "Pet.scala should import iron circe given")
    printGeneratedSources(baseFolder.resolve("pkg"))
  }

  test("composed generator should support tupson + sttp client") {
    val baseFolder = Files.createTempDirectory("openapi4s-tupson-sttp")
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl("sttp_client.yaml"),
      baseFolder = baseFolder,
      basePackage = "pkg",
      models = "tupson",
      framework = "none",
      client = "sttp"
    )
    OpenApiWriter(config).write()
    val generatedFiles = listScalaFiles(baseFolder.resolve("pkg"))
    assert(generatedFiles.exists(_.startsWith("models/")))
    assert(generatedFiles.exists(_.startsWith("clients/PetClient.scala")))
    assert(generatedFiles.exists(_.startsWith("clients/JsonSupport.scala")))
    printGeneratedSources(baseFolder.resolve("pkg"))
  }

  test("--tags should filter only the client generation") {
    val baseFolder = Files.createTempDirectory("openapi4s-tags-filter")
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl("sttp_client.yaml"),
      baseFolder = baseFolder,
      basePackage = "pkg",
      models = "tupson",
      framework = "none",
      client = "sttp",
      tags = Some(List("Pet"))
    )
    OpenApiWriter(config).write()
    val generatedFiles = listScalaFiles(baseFolder.resolve("pkg"))
    // only Pet client is generated
    assert(generatedFiles.exists(_.startsWith("clients/PetClient.scala")))
    assert(!generatedFiles.exists(_.startsWith("clients/StoreClient.scala")))
    assert(!generatedFiles.exists(_.startsWith("clients/DefaultClient.scala")))
    // models are unaffected (all still generated)
    val modelFiles = generatedFiles.filter(_.startsWith("models/"))
    assert(modelFiles.nonEmpty)
    assert(modelFiles.exists(_.endsWith("Pet.scala")))
    assert(modelFiles.exists(_.endsWith("Order.scala")))
    printGeneratedSources(baseFolder.resolve("pkg"))
  }

  test("composed generator should reject unknown client backend") {
    interceptMessage[RuntimeException]("Unknown client backend 'bogus'. Available client backends: 'sttp', 'none'") {
      OpenApiWriter(
        OpenApiWriter.Config(
          url = TestUtils.getResourceUrl("petstore_3.0.0.json"),
          baseFolder = Paths.get("app"),
          basePackage = "pkg",
          models = "circe",
          framework = "none",
          client = "bogus"
        )
      )
    }
  }

  private def listScalaFiles(base: Path): List[String] = {
    if (!Files.exists(base)) List.empty
    else {
      val stream = Files.walk(base)
      try {
        stream
          .iterator()
          .asScala
          .filter(path => Files.isRegularFile(path) && path.getFileName.toString.endsWith(".scala"))
          .map(path => base.relativize(path).toString.replace('\\', '/'))
          .toList
      } finally stream.close()
    }
  }

  private def readGeneratedFile(base: Path, prefix: String): String = {
    val generatedFiles = listScalaFiles(base)
    val relative =
      generatedFiles.find(_.startsWith(prefix)).getOrElse(fail(s"Expected generated file with prefix '$prefix'"))
    Files.readString(base.resolve(relative))
  }

  private def printGeneratedSources(base: Path): Unit = {
    val scalaFiles = listScalaFiles(base).sorted
    scalaFiles.foreach { relativePath =>
      val content = Files.readString(base.resolve(relativePath))
      println(
        s"""**** $relativePath ****
           |$content""".stripMargin
      )
    }
  }
}
