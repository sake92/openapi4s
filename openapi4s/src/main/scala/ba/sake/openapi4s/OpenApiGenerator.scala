package ba.sake.openapi4s

import java.nio.file.Path
import ba.sake.openapi4s.sharaf.SharafGenerator

trait OpenApiGenerator {
  def generate(config: OpenApiGenerator.Config): Unit
}

object OpenApiGenerator {

  private val generators = Map(
    "sharaf" -> new SharafGenerator()
  )

  def apply(name: String): OpenApiGenerator = generators.getOrElse(name.toLowerCase, {
    val available = generators.keys.map(g => s"'${g}'")
    throw new RuntimeException(s"Unknown generator '${name}'. Available generators: ${available}")
  })

  case class Config(
      url: String,
      baseFolder: Path,
      basePackage: String
  )

}
