package ba.sake.openapi4s

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

/** Compiles generated Scala 3 sources with scala-cli (external tool, Scala 3 compiler). */
object CompilationTestUtils {

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
      s"Generated sources failed to compile (scala-cli exit code $exitCode):\n$output"
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

  private def runProcess(bin: String, args: List[String]): (Int, String) = {
    val outFile = Files.createTempFile("openapi4s-scalacli", ".log")
    try {
      val builder = new ProcessBuilder((bin :: args).asJava)
      builder.redirectErrorStream(true)
      builder.redirectOutput(outFile.toFile)
      val process = builder.start()
      val exitCode = process.waitFor()
      (exitCode, Files.readString(outFile))
    } finally Files.deleteIfExists(outFile)
  }
}
