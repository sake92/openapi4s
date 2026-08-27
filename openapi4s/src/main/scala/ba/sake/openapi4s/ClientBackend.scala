package ba.sake.openapi4s

import ba.sake.openapi4s.sttp.SttpClientGenerator
import ba.sake.regenesca.GeneratedFileSource

sealed trait ClientBackendId
object ClientBackendId {
  case object NoClient extends ClientBackendId
  case object Sttp extends ClientBackendId

  val all: List[ClientBackendId] = List(NoClient, Sttp)

  def fromString(s: String): ClientBackendId = s.toLowerCase match {
    case "sttp" => Sttp
    case "none" => NoClient
    case _ =>
      throw new RuntimeException(
        s"Unknown client backend '${s}'. Available client backends: 'sttp', 'none'"
      )
  }
}

trait ClientBackend {
  def id: ClientBackendId
  def supportedModelIds: Set[ModelBackendId]
  def generator(
      config: OpenApiWriter.Config,
      openapiDefinition: OpenApiDefinition,
      modelContract: ModelContract
  ): OpenApiGenerator
}

object ClientBackend {

  val none: ClientBackend = new ClientBackend {
    override val id: ClientBackendId = ClientBackendId.NoClient
    override val supportedModelIds: Set[ModelBackendId] =
      Set(ModelBackendId.NoModel, ModelBackendId.Circe, ModelBackendId.Tupson)
    override def generator(
        config: OpenApiWriter.Config,
        openapiDefinition: OpenApiDefinition,
        modelContract: ModelContract
    ): OpenApiGenerator = new OpenApiGenerator {
      override def generate(): Seq[GeneratedFileSource] = Seq.empty
    }
  }

  val sttp: ClientBackend = new ClientBackend {
    override val id: ClientBackendId = ClientBackendId.Sttp
    override val supportedModelIds: Set[ModelBackendId] = Set(ModelBackendId.Circe, ModelBackendId.Tupson)
    override def generator(
        config: OpenApiWriter.Config,
        openapiDefinition: OpenApiDefinition,
        modelContract: ModelContract
    ): OpenApiGenerator = {
      new SttpClientGenerator(
        config = config,
        openApiDefinition = openapiDefinition,
        modelContract = modelContract
      )
    }
  }

  val byId: Map[ClientBackendId, ClientBackend] = Map(
    ClientBackendId.NoClient -> none,
    ClientBackendId.Sttp -> sttp
  )
}
