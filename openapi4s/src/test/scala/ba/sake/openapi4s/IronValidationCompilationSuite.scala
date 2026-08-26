package ba.sake.openapi4s

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

/** Compiles generated Scala 3 sources with the Scala 3 compiler to make sure they are valid. */
class IronValidationCompilationSuite extends munit.FunSuite {

  test("generated circe + iron sources compile") {
    val baseFolder = generate(
      url = "iron_constraints.json",
      basePackage = "com.example.iron",
      validation = "iron"
    )
    compileGenerated(baseFolder.resolve("com/example/iron"))
  }

  test("generated circe sources compile") {
    val baseFolder = generate(
      url = "petstore_3.0.0.json",
      basePackage = "com.example.plain",
      validation = "none"
    )
    compileGenerated(baseFolder.resolve("com/example/plain"))
  }

  private def generate(url: String, basePackage: String, validation: String): Path = {
    val baseFolder = Files.createTempDirectory(s"openapi4s-compile-${validation}")
    val config = OpenApiWriter.Config(
      url = TestUtils.getResourceUrl(url),
      baseFolder = baseFolder,
      basePackage = basePackage,
      models = "circe",
      framework = "none",
      validation = validation
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
