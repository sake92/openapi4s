package ba.sake.openapi4s

import scala.jdk.CollectionConverters._
import io.swagger.parser.OpenAPIParser

case class OpenApiDefinition(
    namedSchemaDefinitions: NamedSchemaDefinitions,
    pathDefinitions: PathDefinitions
) {
  // make OneOf-s come first!
  def normalized: OpenApiDefinition = {
    val sortedDefs = namedSchemaDefinitions.defs.sortWith { (a, b) =>
      (a.schema, b.schema) match {
        case (_: SchemaDefinition.OneOf, _) => true
        case _                              => false
      }
    }
    this.copy(
      namedSchemaDefinitions = namedSchemaDefinitions.copy(defs = sortedDefs)
    )
  }
}

object OpenApiDefinition {

  def parse(url: String): OpenApiDefinition = {
    val result = new OpenAPIParser().readLocation(url, null, null)
    val openApi = result.getOpenAPI
    val errorMessagesOpt = Option(result.getMessages).map(_.asScala.toSeq).filterNot(_.isEmpty)

    if (openApi == null) {
      val msgs = errorMessagesOpt.getOrElse(Seq.empty).mkString("; ")
      throw new RuntimeException(s"OpenAPI definition at '${url}' is not valid: ${msgs}")
    } else {
      errorMessagesOpt.foreach { errorMessages =>
        val msgs = errorMessages.mkString("; ")
        println(s"OpenAPI definition at '${url}' had issues: ${msgs}")
      }
      val schemaDefinitionResolver = new SchemaDefinitionResolver()
      val namedSchemaDefinitions = Option(openApi.getComponents) match {
        case Some(components) =>
          schemaDefinitionResolver.resolveNamedSchemas(
            Option(components.getSchemas).map(_.asScala).getOrElse(List.empty).toMap
          )
        case None =>
          NamedSchemaDefinitions(Seq.empty)
      }
      val pathsResolver = new PathDefinitionsResolver(schemaDefinitionResolver)
      val pathDefinitions = pathsResolver.resolve(openApi.getPaths)
      OpenApiDefinition(namedSchemaDefinitions, pathDefinitions).normalized
    }
  }
}
