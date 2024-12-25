package ba.sake.openapi4s

import ba.sake.openapi4s.exceptions.{UnsupportedSchemaDefinitionException, UnsupportedSchemaException}

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._
import io.swagger.v3.oas.models.Paths

class PathDefinitionsResolver(
    schemaDefinitionResolver: SchemaDefinitionResolver
) {

  private val ApplicationJson = "application/json"

  def resolve(paths: Paths): PathDefinitions = {
    val pathDefs = Option(paths)
      .map(_.asScala)
      .getOrElse(Map.empty)
      .flatMap { case (pathKey, pathItem) =>
        Option(pathItem.readOperationsMap)
          .map(_.asScala)
          .getOrElse(Map.empty)
          .map { case (method, operation) =>
            /* request */
            val reqBody =
              Option(operation.getRequestBody)
                .flatMap(b => Option(b.getContent.get(ApplicationJson)))
                .flatMap { mediaType =>
                  val bodySchema =
                    schemaDefinitionResolver.resolveSchema(mediaType.getSchema, s"path ${method} '${pathKey}' req body")
                  if (isAllowedBodySchema(bodySchema)) {
                    Some(
                      ReqBodyDefinition(
                        schema = bodySchema,
                        required = operation.getRequestBody.getRequired
                      )
                    )
                  } else {
                    println(
                      s"Request body schema '${bodySchema}' not allowed (probably an unnamed object) " +
                        s"at path ${method} '${pathKey}'. Skipping."
                    )
                    None
                  }

                }
            /* response */
            val resBody = Option(operation.getResponses)
              .map(_.asScala)
              .getOrElse(Map.empty)
              .find { case (responseKey, _) =>
                responseKey.toIntOption
                  .map(s => s >= 200 && s <= 299)
                  .getOrElse(responseKey == "default")
              }
              .flatMap { case (_, apiResponse) =>
                Option(apiResponse.getContent)
                  .map(_.get(ApplicationJson))
                  .flatMap { mediaType =>
                    try {
                      val bodySchema =
                        schemaDefinitionResolver.resolveSchema(
                          mediaType.getSchema,
                          s"path ${method} '${pathKey}' res body"
                        )
                      if (isAllowedBodySchema(bodySchema))
                        Some(ResBodyDefinition(schema = bodySchema))
                      else {
                        println(
                          s"Response body schema '${bodySchema}' not allowed (probably an unnamed object) at path ${method} '${pathKey}'. Skipping."
                        )
                        None
                      }
                    } catch {
                      // skip if "object", no support for structural types etc..
                      case _: UnsupportedSchemaDefinitionException => None
                    }
                  }
              }
            /* path */
            val pathParamsMap = Option(operation.getParameters)
              .map(_.asScala)
              .getOrElse(List.empty)
              .filter(_.getIn == "path")
              .map { param =>
                val paramSchema = schemaDefinitionResolver.resolveSchema(
                  param.getSchema,
                  s"path ${method} '${pathKey}' path param '${param.getName}'"
                )
                if (!isAllowedPathParamSchema(paramSchema))
                  throw new UnsupportedSchemaException(
                    s"Path param schema not allowed: ${paramSchema} (probably an object or array). " +
                      s"Param name: ${param.getName} in ${method} ${pathKey}"
                  )
                param.getName -> paramSchema
              }
              .toMap
            val pathSegments: List[PathSegment] = pathKey
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
              .toList
            /* query */
            val queryParams =
              Option(operation.getParameters)
                .map(_.asScala)
                .getOrElse(List.empty)
                .filter(_.getIn == "query")
                .flatMap { param =>
                  val paramSchema =
                    schemaDefinitionResolver.resolveSchema(
                      param.getSchema,
                      s"path ${method} '${pathKey}' query param '${param.getName}'"
                    )
                  if (isAllowedQueryParamSchema(paramSchema))
                    Some(QueryParam(param.getName, paramSchema, Option(param.getRequired).exists(_.booleanValue)))
                  else {
                    println(
                      s"Query param schema not allowed: ${paramSchema} (probably an unnamed object). " +
                        s"Param name: ${param.getName} in ${method} ${pathKey}. Skipping."
                    )
                    None
                  }
                }
                .toList

            PathDefinition(
              method = method.toString,
              path = pathKey,
              pathSegments = pathSegments,
              queryParams = queryParams,
              reqBody = reqBody,
              resBody = resBody,
              tags = Option(operation.getTags).map(_.asScala.toList).getOrElse(List.empty),
              summary = Option(operation.getSummary).getOrElse(""),
              description = Option(operation.getDescription).getOrElse(""),
              operationId = Option(operation.getOperationId).getOrElse("")
            )
          }
          .toList
      }
      .toList
    PathDefinitions(pathDefs)
  }

  @tailrec
  private def isAllowedPathParamSchema(schema: SchemaDefinition): Boolean = schema match {
    case _: SchemaDefinition.Str           => true
    case _: SchemaDefinition.Password      => true
    case _: SchemaDefinition.Email         => true
    case _: SchemaDefinition.Base64Bytes   => true
    case _: SchemaDefinition.Int32         => true
    case _: SchemaDefinition.Int64         => true
    case _: SchemaDefinition.Num32         => true
    case _: SchemaDefinition.Num64         => true
    case _: SchemaDefinition.Bool          => true
    case _: SchemaDefinition.Uuid          => true
    case _: SchemaDefinition.Date          => true
    case _: SchemaDefinition.DateTime      => true
    case SchemaDefinition.Opt(innerSchema) => isAllowedPathParamSchema(innerSchema)
    case _: SchemaDefinition.Enum          => true
    case SchemaDefinition.Arr(_, _, _, _)  => false
    case _: SchemaDefinition.Ref           => false
    case _: SchemaDefinition.Named         => false
    case _: SchemaDefinition.Obj           => false
    case _: SchemaDefinition.OneOf         => false
    case _: SchemaDefinition.Unknown       => false
  }

  @tailrec
  private def isAllowedQueryParamSchema(schema: SchemaDefinition): Boolean = schema match {
    case _: SchemaDefinition.Str                    => true
    case _: SchemaDefinition.Password               => true
    case _: SchemaDefinition.Email                  => true
    case _: SchemaDefinition.Base64Bytes            => true
    case _: SchemaDefinition.Int32                  => true
    case _: SchemaDefinition.Int64                  => true
    case _: SchemaDefinition.Num32                  => true
    case _: SchemaDefinition.Num64                  => true
    case _: SchemaDefinition.Bool                   => true
    case _: SchemaDefinition.Uuid                   => true
    case _: SchemaDefinition.Date                   => true
    case _: SchemaDefinition.DateTime               => true
    case SchemaDefinition.Opt(innerSchema)          => isAllowedQueryParamSchema(innerSchema)
    case _: SchemaDefinition.Enum                   => true
    case SchemaDefinition.Arr(innerSchema, _, _, _) => isAllowedQueryParamSchema(innerSchema)
    case _: SchemaDefinition.Ref                    => true
    case _: SchemaDefinition.Named                  => true
    case _: SchemaDefinition.Obj                    => false
    case _: SchemaDefinition.OneOf                  => false
    case _: SchemaDefinition.Unknown                => false
  }

  @tailrec
  private def isAllowedBodySchema(schema: SchemaDefinition): Boolean = schema match {
    case _: SchemaDefinition.Str                    => true
    case _: SchemaDefinition.Password               => true
    case _: SchemaDefinition.Email                  => true
    case _: SchemaDefinition.Base64Bytes            => true
    case _: SchemaDefinition.Int32                  => true
    case _: SchemaDefinition.Int64                  => true
    case _: SchemaDefinition.Num32                  => true
    case _: SchemaDefinition.Num64                  => true
    case _: SchemaDefinition.Bool                   => true
    case _: SchemaDefinition.Uuid                   => true
    case _: SchemaDefinition.Date                   => true
    case _: SchemaDefinition.DateTime               => true
    case SchemaDefinition.Opt(innerSchema)          => isAllowedBodySchema(innerSchema)
    case _: SchemaDefinition.Enum                   => true
    case SchemaDefinition.Arr(innerSchema, _, _, _) => isAllowedBodySchema(innerSchema)
    case _: SchemaDefinition.Ref                    => true
    case _: SchemaDefinition.Named                  => true
    case _: SchemaDefinition.Obj                    => false
    case _: SchemaDefinition.OneOf                  => false
    case _: SchemaDefinition.Unknown                => false
  }

}
