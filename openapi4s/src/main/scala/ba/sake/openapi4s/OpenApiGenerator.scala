package ba.sake.openapi4s

import ba.sake.regenesca.GeneratedFileSource
import java.nio.file.Path
import ba.sake.openapi4s.sharaf.SharafGenerator

trait OpenApiGenerator {
  def generate(config: OpenApiGenerator.Config): Unit
}

object OpenApiGenerator {

  private val generators = Map(
    "sharaf" -> new SharafGenerator()
  )

  def apply(name: String) = generators.get(name.toLowerCase).getOrElse {
    val available = generators.keys.map(g => s"'${g}'")
    throw new RuntimeException(s"Unknown generator '${name}'. Available generators: ${available}")
  }

  case class Config(
      url: String,
      baseFolder: Path,
      basePackage: String
  )

}
