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
      validation: String = "none",
      @arg(doc = "Client backend: 'sttp' or 'none'. If unset, defaults to 'none'.")
      client: String = "none",
      @arg(doc = "Comma-separated tags to generate clients for. Currently applies only to clients.")
      tags: String = ""
  ) = {
    val writer = OpenApiWriter(
      config = OpenApiWriter.Config(
        url = url,
        baseFolder = Paths.get(baseFolder),
        basePackage = basePackage,
        models = models,
        framework = framework,
        validation = validation,
        client = client,
        tags = tags.split(",").toList.map(_.trim).filter(_.nonEmpty) match {
          case Nil  => None
          case list => Some(list)
        }
      )
    )
    writer.write()
  }
  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
