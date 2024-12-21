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
    val errorMessages = result.getMessages
    if (errorMessages != null) {
      errorMessages.forEach(println)
    }

    if (openApi == null) {
      throw new RuntimeException(s"OpenAPI definition at '${url}' is not valid")
    } else {
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
