package ba.sake.openapi4s

case class PathDefinitions(
    defs: List[PathDefinition]
)

case class PathDefinition(
    method: String,
    path: String,
    pathSegments: List[PathSegment],
    queryParams: List[QueryParam],
    reqBody: Option[ReqBodyDefinition],
    resBody: Option[ResBodyDefinition],
    tags: List[String],
    summary: String,
    description: String,
    operationId: String
) {
  def getTag: String =
    tags.headOption.getOrElse("Default")
}

sealed abstract class PathSegment
object PathSegment {
  case class Literal(value: String) extends PathSegment
  case class Param(name: String, schema: SchemaDefinition) extends PathSegment
}

case class QueryParam(name: String, schema: SchemaDefinition, required: Boolean)

case class ReqBodyDefinition(schema: SchemaDefinition, required: Boolean)

case class ResBodyDefinition(schema: SchemaDefinition)
