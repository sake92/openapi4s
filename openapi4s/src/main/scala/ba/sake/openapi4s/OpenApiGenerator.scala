package ba.sake.openapi4s

import java.nio.file.Path
import ba.sake.openapi4s.http4s.Http4sGenerator
import ba.sake.openapi4s.sharaf.SharafGenerator

trait OpenApiGenerator {
  def generate(): Unit
}

object OpenApiGenerator {

  def apply(name: String, config: Config): OpenApiGenerator = {
    val openapiDefinition = OpenApiDefinition.parse(config.url)
    name.toLowerCase match {
      case "http4s" => new Http4sGenerator(config, openapiDefinition)
      case "sharaf" => new SharafGenerator(config, openapiDefinition)
      case other    => throw new RuntimeException(s"Unknown generator '${other}'. Available generators: 'http4s', 'sharaf'")
    }
  }

  case class Config(
      url: String,
      baseFolder: Path,
      basePackage: String
  )

}
