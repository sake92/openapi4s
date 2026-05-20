package ba.sake.openapi4s.cli

import ba.sake.openapi4s.OpenApiGenerator

import java.nio.file.Paths
import mainargs.{main, arg, ParserForMethods}

object OpenApi4sMain {

  @main
  def run(
      @arg(doc = "Model backend: 'circe', 'tupson' or 'none'. Default is 'tupson'")
      models: String = "",
      @arg(doc = "Framework backend: 'http4s', 'sharaf' or 'none'. Default is 'sharaf'")
      framework: String = "",
      @arg(doc = "deprecated legacy generator mapping: 'sharaf' => models=tupson+framework=sharaf, 'http4s' => models=circe+framework=http4s")
      generator: String = "",
      @arg(doc = "OpenAPI URL or file path. Default is 'openapi.json'")
      url: String = "openapi.json",
      @arg(doc = "Base folder for generated sources. Default is 'src/main/scala'")
      baseFolder: String = "src/main/scala",
      @arg(doc = "Base package for generated sources")
      basePackage: String
  ) = {
    val (mappedModels, mappedFramework) = generator.toLowerCase match {
      case ""       => ("tupson", "sharaf")
      case "sharaf" => ("tupson", "sharaf")
      case "http4s" => ("circe", "http4s")
      case other =>
        throw new RuntimeException(s"Unknown generator '${other}'. Available generators: 'http4s', 'sharaf'")
    }
    val finalModels = if (models.nonEmpty) models else mappedModels
    val finalFramework = if (framework.nonEmpty) framework else mappedFramework
    if (generator.nonEmpty) {
      System.err.println("WARNING: '--generator' is deprecated. Prefer '--models' and '--framework'.")
    }
    val openApiGenerator = OpenApiGenerator(
      config = OpenApiGenerator.Config(
        url = url,
        baseFolder = Paths.get(baseFolder),
        basePackage = basePackage,
        models = finalModels,
        framework = finalFramework
      )
    )
    openApiGenerator.generate()
  }
  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
