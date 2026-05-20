package ba.sake.openapi4s

import ba.sake.openapi4s.OpenApiGenerator._
import ba.sake.openapi4s.http4s.Http4sGenerator
import ba.sake.openapi4s.sharaf.SharafGenerator
import ba.sake.regenesca.GeneratedFileSource

object ModelBackends {
  private val ModelsPrefix = "models/"

  val circe: ModelBackend = new ModelBackend {
    override val id: String = "circe"
    override val flavor: ModelFlavor = ModelFlavor.Circe
    override val imports: ModelImportContract = ModelImportContracts.circe
    override def generateSources(config: Config, openapiDefinition: OpenApiDefinition): Seq[GeneratedFileSource] = {
      new Http4sGenerator(
        config = config,
        openApiDefinition = openapiDefinition,
        modelFileImports = imports.modelFileImports,
        frameworkModelImports = List.empty
      ).generateSources.filter(_.file.toString.startsWith(ModelsPrefix))
    }
  }

  val tupson: ModelBackend = new ModelBackend {
    override val id: String = "tupson"
    override val flavor: ModelFlavor = ModelFlavor.Tupson
    override val imports: ModelImportContract = ModelImportContracts.tupson
    override def generateSources(config: Config, openapiDefinition: OpenApiDefinition): Seq[GeneratedFileSource] = {
      new SharafGenerator(
        config = config,
        openApiDefinition = openapiDefinition,
        modelFileImports = imports.modelFileImports,
        frameworkModelImports = List.empty
      ).generateSources.filter(_.file.toString.startsWith(ModelsPrefix))
    }
  }

  val byId: Map[String, ModelBackend] = Map(
    "circe" -> circe,
    "tupson" -> tupson
  )
}
