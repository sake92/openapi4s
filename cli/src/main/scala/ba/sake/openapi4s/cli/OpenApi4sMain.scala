package ba.sake.openapi4s.cli

import ba.sake.openapi4s.OpenApiWriter

import java.nio.file.Paths
import mainargs.{main, arg, ParserForMethods}

object OpenApi4sMain {

  @main
  def run(
      @arg(doc = "Model backend: 'circe' or 'tupson'. Defaults to 'tupson'.")
      models: String = "tupson",
      @arg(doc = "Server framework backend: 'http4s' or 'sharaf'. If unset, no server is generated.")
      framework: Option[String] = None,
      @arg(doc = "Client backend: 'sttp'. If unset, no client is generated.")
      client: Option[String] = None,
      @arg(doc = "OpenAPI URL or file path. Default is 'openapi.json'")
      url: String = "openapi.json",
      @arg(doc = "Base folder for generated sources. Default is 'src/main/scala'")
      baseFolder: String = "src/main/scala",
      @arg(doc = "Base package for generated sources")
      basePackage: String,
      @arg(doc = "Validation backend: 'none', 'iron' or 'validson'. If unset, defaults to 'none'.")
      validation: String = "none",
      @arg(doc = "Comma-separated tags to generate clients for. Currently applies only to clients.")
      tags: String = ""
  ) = {
    val (modelsResolved, frameworkResolved, clientResolved) = resolveBackends(models, framework, client)
    val writer = OpenApiWriter(
      config = OpenApiWriter.Config(
        url = url,
        baseFolder = Paths.get(baseFolder),
        basePackage = basePackage,
        models = modelsResolved,
        framework = frameworkResolved,
        validation = validation,
        client = clientResolved,
        tags = tags.split(",").toList.map(_.trim).filter(_.nonEmpty) match {
          case Nil  => None
          case list => Some(list)
        }
      )
    )
    writer.write()
  }

  /** Resolves the optional `--framework` and `--client` flags to library backend ids. `--models` is mandatory (always
    * generated). The library's internal "none" is not accepted from the CLI anymore — to generate no server/client,
    * omit the flag.
    */
  private[cli] def resolveBackends(
      models: String,
      framework: Option[String],
      client: Option[String]
  ): (String, String, String) = {

    val m = models.toLowerCase match {
      case "circe"  => "circe"
      case "tupson" => "tupson"
      case "none" =>
        throw new RuntimeException(
          "Invalid --models value 'none'. Models are mandatory; use 'circe' or 'tupson'."
        )
      case other =>
        throw new RuntimeException(
          s"Unknown model backend '$other'. Available model backends: 'circe', 'tupson'."
        )
    }

    val f = framework.map(_.toLowerCase) match {
      case None           => "none"
      case Some("http4s") => "http4s"
      case Some("sharaf") => "sharaf"
      case Some("none") =>
        throw new RuntimeException(
          "Invalid --framework value 'none'. To generate no server, omit --framework entirely. Available server frameworks: 'http4s', 'sharaf'."
        )
      case Some(other) =>
        throw new RuntimeException(
          s"Unknown framework backend '$other'. Available framework backends: 'http4s', 'sharaf'."
        )
    }

    val c = client.map(_.toLowerCase) match {
      case None         => "none"
      case Some("sttp") => "sttp"
      case Some("none") =>
        throw new RuntimeException(
          "Invalid --client value 'none'. To generate no client, omit --client entirely. Available client backends: 'sttp'."
        )
      case Some(other) =>
        throw new RuntimeException(
          s"Unknown client backend '$other'. Available client backends: 'sttp'."
        )
    }

    (m, f, c)
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
}
