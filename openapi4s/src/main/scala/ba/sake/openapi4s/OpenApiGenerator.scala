package ba.sake.openapi4s

import java.nio.file.Path
import ba.sake.openapi4s.sharaf.SharafGenerator

trait OpenApiGenerator {
  def generate(): Unit
}

object OpenApiGenerator {

  def apply(name: String, config: Config): OpenApiGenerator = {
    val openapiDefinition = OpenApiDefinition.parse(config.url)
    name.toLowerCase match {
      case "sharaf" => new SharafGenerator(config, openapiDefinition)
      case other    => throw new RuntimeException(s"Unknown generator '${other}'. Available generators: 'sharaf'")
    }
  }

  case class Config(
      url: String,
      baseFolder: Path,
      basePackage: String
  )

}
