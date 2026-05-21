package ba.sake.openapi4s

import ba.sake.openapi4s.OpenApiWriter.Config
import ba.sake.openapi4s.http4s.Http4sGenerator
import ba.sake.openapi4s.sharaf.SharafGenerator
import ba.sake.regenesca.GeneratedFileSource

sealed trait FrameworkBackendId
object FrameworkBackendId {
  case object NoFramework extends FrameworkBackendId
  case object Http4s extends FrameworkBackendId
  case object Sharaf extends FrameworkBackendId

  val all: List[FrameworkBackendId] = List(NoFramework, Http4s, Sharaf)

  def fromString(s: String): FrameworkBackendId = s.toLowerCase match {
    case "http4s" => Http4s
    case "sharaf" => Sharaf
    case "none"   => NoFramework
    case _ =>
      throw new RuntimeException(
        s"Unknown framework backend '${s}'. Available framework backends: 'http4s', 'sharaf', 'none'"
      )
  }
}

trait FrameworkBackend {
  def id: FrameworkBackendId
  def supportedModelIds: Set[ModelBackendId]
  def generator(
      config: OpenApiWriter.Config,
      openapiDefinition: OpenApiDefinition,
      modelContract: ModelContract
  ): OpenApiGenerator
}

object FrameworkBackend {

  val none: FrameworkBackend = new FrameworkBackend {
    override val id: FrameworkBackendId = FrameworkBackendId.NoFramework
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

  val http4s: FrameworkBackend = new FrameworkBackend {
    override val id: FrameworkBackendId = FrameworkBackendId.Http4s
    override val supportedModelIds: Set[ModelBackendId] = Set(ModelBackendId.Circe)
    override def generator(
        config: OpenApiWriter.Config,
        openapiDefinition: OpenApiDefinition,
        modelContract: ModelContract
    ): OpenApiGenerator = {
      new Http4sGenerator(
        config = config,
        openApiDefinition = openapiDefinition,
        frameworkModelImports = modelContract.imports.frameworkImports(id)
      )
    }
  }

  val sharaf: FrameworkBackend = new FrameworkBackend {
    override val id: FrameworkBackendId = FrameworkBackendId.Sharaf
    override val supportedModelIds: Set[ModelBackendId] = Set(ModelBackendId.Tupson)
    override def generator(
        config: OpenApiWriter.Config,
        openapiDefinition: OpenApiDefinition,
        modelContract: ModelContract
    ): OpenApiGenerator = {
      new SharafGenerator(
        config = config,
        openApiDefinition = openapiDefinition,
        frameworkModelImports = modelContract.imports.frameworkImports(id)
      )
    }
  }

  val byId: Map[FrameworkBackendId, FrameworkBackend] = Map(
    FrameworkBackendId.NoFramework -> none,
    FrameworkBackendId.Http4s -> http4s,
    FrameworkBackendId.Sharaf -> sharaf
  )
}
