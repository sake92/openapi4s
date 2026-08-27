package ba.sake.openapi4s

import java.nio.file.{Files, Path}
import scala.concurrent.duration.Duration

/**
 * Compiles generated tupson Scala 3 sources with scala-cli (and tupson on the classpath)
 * to make sure named tuples, literal unions, union types and maps typecheck.
 */
class TupsonCompilationSuite extends munit.FunSuite {

  // first CI run resolves deps, so the default 30s timeout is not enough
  override def munitTimeout: Duration = Duration(10, "min")

  test("generated tupson sources compile (3.0 features)") {
    val baseFolder = generate("tupson_features.yaml", "com.example.features")
    CompilationTestUtils.compileGenerated(
      baseFolder.resolve("com/example/features"),
      scalaVersion = "3.7.3",
      dependencies = Seq("ba.sake::tupson:0.20.0", "ba.sake::validson:0.19.0")
    )
  }

  test("generated tupson sources compile (3.1 const)") {
    val baseFolder = generate("tupson_features_31.yaml", "com.example.const")
    CompilationTestUtils.compileGenerated(
      baseFolder.resolve("com/example/const"),
      scalaVersion = "3.7.3",
      dependencies = Seq("ba.sake::tupson:0.20.0", "ba.sake::validson:0.19.0")
    )
  }

  private def generate(url: String, basePackage: String): Path = {
    val baseFolder = Files.createTempDirectory("openapi4s-compile-tupson")
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl(url),
      baseFolder = baseFolder,
      basePackage = basePackage,
      models = "tupson",
      framework = "none",
      validation = "none"
    )
    OpenApiWriter(config).write()
    baseFolder
  }
}
