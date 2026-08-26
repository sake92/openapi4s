package ba.sake.openapi4s

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

/**
 * Compiles generated tupson Scala 3 sources with the Scala 3 compiler (and tupson on the classpath)
 * to make sure named tuples, literal unions, union types and maps typecheck.
 */
class TupsonCompilationSuite extends munit.FunSuite {

  test("generated tupson sources compile (3.0 features)") {
    val baseFolder = generate("tupson_features.yaml", "com.example.features")
    compileGenerated(baseFolder.resolve("com/example/features"))
  }

  test("generated tupson sources compile (3.1 const)") {
    val baseFolder = generate("tupson_features_31.yaml", "com.example.const")
    compileGenerated(baseFolder.resolve("com/example/const"))
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

  private def compileGenerated(base: Path): Unit = {
    val scalaFiles = listScalaFiles(base)
    assert(scalaFiles.nonEmpty, s"No generated Scala files found under: $base")

    val outDir = Files.createTempDirectory("openapi4s-compile-out")
    val classpath = System.getProperty("java.class.path")
    val args = Array(
      "-classpath",
      classpath,
      "-d",
      outDir.toString,
      "-color:never"
    ) ++ scalaFiles.map(f => base.resolve(f).toString)

    val reporter = dottyMainProcess(args)
    assert(
      !reporterHasErrors(reporter),
      s"Generated sources failed to compile:\n${generatedSourcesLog(base)}"
    )
  }

  // dotc is invoked via reflection because this test module is Scala 2.13
  // and cannot reference Scala 3 classes directly (no TASTy reader).
  private def dottyMainProcess(args: Array[String]): AnyRef = {
    val mainCls = Class.forName("dotty.tools.dotc.Main")
    val process = mainCls.getMethod("process", classOf[Array[String]])
    process.invoke(null, args)
  }

  private def reporterHasErrors(reporter: AnyRef): Boolean =
    reporter.getClass.getMethod("hasErrors").invoke(reporter).asInstanceOf[Boolean]

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
          .sorted
      } finally stream.close()
    }
  }

  private def generatedSourcesLog(base: Path): String =
    listScalaFiles(base)
      .map { relativePath =>
        s"**** $relativePath ****\n${Files.readString(base.resolve(relativePath))}"
      }
      .mkString("\n")
}
