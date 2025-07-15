import ba.sake.openapi4s.OpenApiGenerator

import java.nio.file.Paths
import mainargs.{main, arg, ParserForMethods}

object OpenApi4sMain {

  @main
  def run(
      @arg(doc = "Generator name: 'sharaf' or 'http4s'. Default is 'sharaf'")
      generator: String = "sharaf",
      @arg(doc = "OpenAPI URL or file path. Default is 'openapi.json'")
      url: String = "openapi.json",
      @arg(doc = "Base folder for generated sources. Default is 'src/main/scala'")
      baseFolder: String = "src/main/scala",
      @arg(doc = "Base package for generated sources")
      basePackage: String
  ) = {
    val openApiGenerator = OpenApiGenerator(
      name = generator,
      config = OpenApiGenerator.Config(
        url = url,
        baseFolder = Paths.get(baseFolder),
        basePackage = basePackage
      )
    )
    openApiGenerator.generate()
  }
  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
