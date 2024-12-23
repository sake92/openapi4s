package ba.sake.openapi4s

import java.nio.file.Paths

object TestUtils {
  def getResourceUrl(baseName: String): String = {
    getClass.getClassLoader.getResource(baseName).toString
  }
}
