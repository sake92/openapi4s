package ba.sake.openapi4s

import ba.sake.regenesca.GeneratedFileSource

object TestUtils {
  def printGeneratedSources(label: String, sources: Seq[GeneratedFileSource]): Unit = {
    sources.sortBy(_.file.toString).foreach { source =>
      println(s"\n// GENERATED: $label / ${source.file}")
      println(source.source.syntax)
    }
  }

  def getResourceUrl(baseName: String): String = {
    getClass.getClassLoader.getResource(baseName).toString
  }
}
