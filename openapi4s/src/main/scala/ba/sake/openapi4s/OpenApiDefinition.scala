package ba.sake.openapi4s

import scala.jdk.CollectionConverters._
import io.swagger.parser.OpenAPIParser

case class OpenApiDefinition(
    namedSchemaDefinitions: NamedSchemaDefinitions,
    pathDefinitions: PathDefinitions
)

object OpenApiDefinition {

  def parse(url: String): OpenApiDefinition = {
    val result = new OpenAPIParser().readLocation(url, null, null)
    val openApi = result.getOpenAPI
    val errorMessagesOpt = Option (result.getMessages).map(_.asScala.toSeq).filterNot(_.isEmpty)

    if (openApi == null) {
      val msgs = errorMessagesOpt.getOrElse(Seq.empty).mkString("; ")
      throw new RuntimeException(s"OpenAPI definition at '${url}' is not valid: ${msgs}")
    } else {
      errorMessagesOpt.foreach {errorMessages =>
        val msgs = errorMessages.mkString("; ")
        println( s"OpenAPI definition at '${url}' had issues: ${msgs}")
      }
      val schemaDefinitionResolver = new SchemaDefinitionResolver()
      val namedSchemaDefinitions = Option(openApi.getComponents) match {
        case Some(components) =>
          schemaDefinitionResolver.resolveNamedSchemas(components.getSchemas.asScala.toMap)
        case None =>
          NamedSchemaDefinitions(Seq.empty)
      }
      val pathsResolver = new PathsResolver(schemaDefinitionResolver)
      val pathDefinitions = pathsResolver.resolve(openApi.getPaths)
      OpenApiDefinition(namedSchemaDefinitions, pathDefinitions)
    }
  }
}
