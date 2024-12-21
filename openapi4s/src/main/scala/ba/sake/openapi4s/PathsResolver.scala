package ba.sake.openapi4s

import scala.jdk.CollectionConverters._
import io.swagger.v3.oas.models.Paths
import io.swagger.v3.oas.models.PathItem
import io.swagger.v3.oas.models.Operation
import io.swagger.v3.oas.models.media.MediaType

class PathsResolver(
    schemaDefinitionResolver: SchemaDefinitionResolver
) {

  private val ApplicationJson = "application/json"

  def resolve(paths: Paths): PathDefinitions = {
    val pathDefs = paths.asScala.flatMap { case (pathKey, pathItem) =>
      pathItem.readOperationsMap.asScala.map { case (method, operation) =>
        /* request */
        val reqBody =
          Option(operation.getRequestBody)
            .flatMap(b => Option(b.getContent.get(ApplicationJson)))
            .map { mediaType =>
              val bodySchema =
                schemaDefinitionResolver.resolveSchema(mediaType.getSchema)
              ReqBodyDefinition(
                schema = bodySchema,
                required = operation.getRequestBody.getRequired
              )
            }
        /* response */
        val resBody = operation.getResponses.asScala
          .find { case (responseKey, _) =>
            responseKey.toIntOption
              .map(s => s >= 200 && s <= 299)
              .getOrElse(responseKey == "default")
          }
          .flatMap { case (responseKey, apiResponse) =>
            Option(apiResponse.getContent)
              .map(_.get(ApplicationJson))
              .flatMap { mediaType =>
                try {
                  val bodySchema =
                    schemaDefinitionResolver.resolveSchema(mediaType.getSchema)
                  Some(ResBodyDefinition(schema = bodySchema))
                } catch {
                  // skip if "object", no support for structural types etc..
                  case _: UnsupportedSchemaTypeException => None
                }
              }
          }
        /* path */
        val pathParamsMap = Option(operation.getParameters)
          .map(_.asScala)
          .getOrElse(Seq.empty)
          .filter(_.getIn == "path")
          .map { param =>
            val paramSchema =
              schemaDefinitionResolver.resolveSchema(param.getSchema)
            param.getName -> paramSchema
          }
          .toMap
        val pathSegments: Seq[PathSegment] = pathKey
          .dropWhile(_ == '/')
          .split("/")
          .map { rawSegment =>
            if (rawSegment.startsWith("{")) {
              val pathParamName = rawSegment
                .dropWhile(_ == '{')
                .reverse
                .dropWhile(_ == '}')
                .reverse
              val pathParamSchema = pathParamsMap(pathParamName)
              PathSegment.Param(pathParamName, pathParamSchema)
            } else {
              PathSegment.Literal(rawSegment)
            }
          }
          .toSeq
        /* query */
        val queryParams =
          Option(operation.getParameters)
            .map(_.asScala)
            .getOrElse(Seq.empty)
            .filter(_.getIn == "query")
            .map { param =>
              val paramSchema =
                schemaDefinitionResolver.resolveSchema(param.getSchema)
              QueryParam(param.getName, paramSchema, param.getRequired)
            }
            .toSeq

        PathDefinition(
          method = method.toString,
          path = pathKey,
          pathSegments = pathSegments,
          queryParams = queryParams,
          reqBody = reqBody,
          resBody = resBody,
          tags =
            Option(operation.getTags).map(_.asScala.toSeq).getOrElse(Seq.empty),
          summary = Option(operation.getSummary).getOrElse(""),
          description = Option(operation.getDescription).getOrElse(""),
          operationId = Option(operation.getOperationId).getOrElse("")
        )
      }.toSeq
    }.toSeq
    PathDefinitions(pathDefs)
  }

}
