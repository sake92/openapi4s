package ba.sake.openapi4s

import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters._

/** Compiles generated Scala 3 sources with scala-cli (external tool, Scala 3 compiler). */
object CompilationTestUtils {

  private val CompileTimeoutMinutes = 60L
  private val LogPreviewChars = 4000

  // generated github/jira models are huge; the scala-cli compiler JVM needs a big heap.
  // overridable via env, e.g. OPENAPI4S_COMPILE_XMX=4g
  private val CompileXmx = sys.env.getOrElse("OPENAPI4S_COMPILE_XMX", "6g")

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

  private def runProcess(bin: String, args: List[String]): (Int, String) = {
    val outFile = Files.createTempFile("openapi4s-scalacli", ".log")
    try {
      val builder = new ProcessBuilder((bin :: args).asJava)
      builder.redirectErrorStream(true)
      builder.redirectOutput(outFile.toFile)
      // the compiler JVM (spawned by scala-cli) picks this up and keeps heap off mill's test JVM
      builder.environment().put("JDK_JAVA_OPTIONS", s"-Xmx$CompileXmx")
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
