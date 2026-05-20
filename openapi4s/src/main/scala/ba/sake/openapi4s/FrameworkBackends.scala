package ba.sake.openapi4s

import ba.sake.openapi4s.OpenApiGenerator.{Config, ModelFlavor, ModelContract}
import ba.sake.openapi4s.http4s.Http4sGenerator
import ba.sake.openapi4s.sharaf.SharafGenerator
import ba.sake.regenesca.GeneratedFileSource

trait FrameworkBackend {
  def id: String
  def requiredModelFlavor: ModelFlavor
  def generateSources(
      config: Config,
      openapiDefinition: OpenApiDefinition,
      modelContract: ModelContract
  ): Seq[GeneratedFileSource]
}

object FrameworkBackends {
  private val RoutesPrefix = "routes/"
  private val ControllersPrefix = "controllers/"

  val http4s: FrameworkBackend = new FrameworkBackend {
    override val id: String = "http4s"
    override val requiredModelFlavor: ModelFlavor = ModelFlavor.Circe
    override def generateSources(
        config: Config,
        openapiDefinition: OpenApiDefinition,
        modelContract: ModelContract
    ): Seq[GeneratedFileSource] = {
      new Http4sGenerator(
        config = config,
        openApiDefinition = openapiDefinition,
        modelFileImports = modelContract.imports.modelFileImports,
        frameworkModelImports = modelContract.imports.frameworkImports(id)
      ).generateSources.filter(_.file.toString.startsWith(RoutesPrefix))
    }
  }

  val sharaf: FrameworkBackend = new FrameworkBackend {
    override val id: String = "sharaf"
    override val requiredModelFlavor: ModelFlavor = ModelFlavor.Tupson
    override def generateSources(
        config: Config,
        openapiDefinition: OpenApiDefinition,
        modelContract: ModelContract
    ): Seq[GeneratedFileSource] = {
      new SharafGenerator(
        config = config,
        openApiDefinition = openapiDefinition,
        modelFileImports = modelContract.imports.modelFileImports,
        frameworkModelImports = modelContract.imports.frameworkImports(id)
      ).generateSources.filter(_.file.toString.startsWith(ControllersPrefix))
    }
  }

  val byId: Map[String, FrameworkBackend] = Map(
    "http4s" -> http4s,
    "sharaf" -> sharaf
  )
}
