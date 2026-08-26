package ba.sake.openapi4s.cli

import ba.sake.openapi4s.OpenApiWriter

import java.nio.file.Paths
import mainargs.{main, arg, ParserForMethods}

object OpenApi4sMain {

  @main
  def run(
      @arg(doc = "Model backend: 'circe', 'tupson' or 'none'. If unset, defaults to 'tupson'.")
      models: String = "tupson",
      @arg(doc = "Framework backend: 'http4s', 'sharaf' or 'none'. If unset, defaults to 'sharaf'.")
      framework: String = "sharaf",
      @arg(doc = "OpenAPI URL or file path. Default is 'openapi.json'")
      url: String = "openapi.json",
      @arg(doc = "Base folder for generated sources. Default is 'src/main/scala'")
      baseFolder: String = "src/main/scala",
      @arg(doc = "Base package for generated sources")
      basePackage: String,
      @arg(doc = "Validation backend: 'none', 'iron' or 'validson'. If unset, defaults to 'none'.")
      validation: String = "none"
  ) = {
    val writer = OpenApiWriter(
      config = OpenApiWriter.Config(
        url = url,
        baseFolder = Paths.get(baseFolder),
        basePackage = basePackage,
        models = models,
        framework = framework,
        validation = validation
      )
    )
    writer.write()
  }
  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
