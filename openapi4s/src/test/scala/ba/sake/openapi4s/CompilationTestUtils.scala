package ba.sake.openapi4s

import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters._

/** Compiles generated Scala 3 sources with scala-cli (external tool, Scala 3 compiler). */
object CompilationTestUtils {

  private val CompileTimeoutMinutes = 60L
  private val LogPreviewChars = 4000

  /** Compiles all .scala files under `base` with scala-cli. Fails the assertion on non-zero exit. */
  def compileGenerated(base: Path, scalaVersion: String, dependencies: Seq[String]): Unit = {
    val scalaFiles = listScalaFiles(base)
    assert(scalaFiles.nonEmpty, s"No generated Scala files found under: $base")

    val scalaCliBin = sys.env.getOrElse("SCALA_CLI_BIN", "scala-cli")
    val args = List("compile", "--server=false", s"--scala=$scalaVersion") ++
      dependencies.flatMap(d => List("--dependency", d)) ++
      scalaFiles.map(f => base.resolve(f).toString)

    val (exitCode, output) = runProcess(scalaCliBin, args)
    assert(
      exitCode == 0,
      s"Generated sources failed to compile (scala-cli exit code $exitCode):\n${preview(output)}"
    )
  }

  def listScalaFiles(base: Path): List[String] = {
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

  /** tupson has no JsonRW[LocalDate]; generated models with date fields need this instance to compile. */
  def writeLocalDateJsonRW(modelsDir: Path, basePackage: String): Unit = {
    val content =
      s"""package ${basePackage}.models
         |
         |import java.time.LocalDate
         |import org.typelevel.jawn.ast.{JString, JValue}
         |import ba.sake.tupson.*
         |
         |given JsonRW[LocalDate] with {
         |  override def write(value: LocalDate): JValue = JString(value.toString)
         |  override def parse(path: String, jValue: JValue): LocalDate = jValue match
         |    case JString(s) => LocalDate.parse(s)
         |    case other =>
         |      throw ParsingException(
         |        ParseError(path, "should be a date string", Some(other.render().take(100)))
         |      )
         |}
         |""".stripMargin
    Files.writeString(modelsDir.resolve("LocalDateJsonRW.scala"), content)
  }

  private def runProcess(bin: String, args: List[String]): (Int, String) = {
    val outFile = Files.createTempFile("openapi4s-scalacli", ".log")
    try {
      val builder = new ProcessBuilder((bin :: args).asJava)
      builder.redirectErrorStream(true)
      builder.redirectOutput(outFile.toFile)
      val process = builder.start()
      val finished = process.waitFor(CompileTimeoutMinutes, TimeUnit.MINUTES)
      if (!finished) {
        process.destroyForcibly()
        (1, s"scala-cli compile timed out after ${CompileTimeoutMinutes} minutes")
      } else {
        (process.exitValue(), Files.readString(outFile))
      }
    } finally Files.deleteIfExists(outFile)
  }

  /** Keeps the assertion message bounded even when dotc emits megabytes of errors. */
  private def preview(output: String): String = {
    if (output.length <= LogPreviewChars) output
    else output.take(LogPreviewChars / 2) + "\n... [truncated] ...\n" + output.takeRight(LogPreviewChars / 2)
  }
}
