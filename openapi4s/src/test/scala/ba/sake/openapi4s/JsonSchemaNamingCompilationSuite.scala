package ba.sake.openapi4s

import java.nio.file.{Files, Paths}
import scala.concurrent.duration.Duration

class JsonSchemaNamingCompilationSuite extends munit.FunSuite {
  override def munitTimeout: Duration = Duration(10, "min")

  for (backend <- List("tupson", "circe")) {
    test(s"$backend emits and compiles top-level JSON Schema definition names") {
      val target = Files.createTempDirectory(s"json-schema-naming-$backend")
      val folder = Paths.get(getClass.getClassLoader.getResource("json-schema/naming").toURI)
      OpenApiWriter(
        OpenApiWriter.Config(
          url = folder.toString,
          baseFolder = target,
          basePackage = "naming.models",
          models = backend,
          framework = "none",
          validation = "none"
        )
      ).write()
      val models = target.resolve("naming/models/models")
      val files = CompilationTestUtils.listScalaFiles(models)
      assert(files.contains("FileNode.scala"), files)
      assert(files.contains("DeclarationSurface.scala"), files)
      assert(!files.exists(_.startsWith("Artifact__")), files)
      assert(Files.readString(models.resolve("ExportGraph.scala")).contains("files: Seq[FileNode]"))
      val dependencies = if (backend == "tupson") {
        Seq("ba.sake::tupson:0.31.0", "ba.sake::validson:0.20.0")
      } else {
        Seq("io.circe::circe-core:0.14.17", "io.circe::circe-generic:0.14.17", "ba.sake::validson:0.20.0")
      }
      CompilationTestUtils.compileGenerated(models, "3.9.0", dependencies)
    }
  }
}
