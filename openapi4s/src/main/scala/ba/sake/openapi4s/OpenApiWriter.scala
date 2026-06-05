package ba.sake.openapi4s

import java.nio.file.Path
import ba.sake.regenesca.{GeneratedFileSource, RegenescaGenerator, SourceMerger}
import scala.meta.contrib.implicits.TreeExtensions
import scala.meta.dialects.Scala34

class OpenApiWriter(
    config: OpenApiWriter.Config,
    modelBackend: ModelBackend,
    frameworkBackend: FrameworkBackend,
    validationBackend: ValidationBackend
) {
  private val openapiDefinition = OpenApiDefinition.parse(config.url)
  private val merger = SourceMerger(mergeDefBodies = true)
  private val regenescaGenerator = RegenescaGenerator(merger)

  def write(): Seq[GeneratedFileSource] = {
    println(
      s"Started generating OpenApi for '${config.url}' with models='${config.models}', framework='${config.framework}', validation='${config.validation}' ..."
    )
    val (validationSources, validationTypeMap) = validationBackend.generate(config, openapiDefinition)
    val modelSources = modelBackend.generator(config, openapiDefinition, validationTypeMap).generate()
    val modelContract = modelBackend.contract(config)
    val frameworkSources = frameworkBackend.generator(config, openapiDefinition, modelContract).generate()
    val packagePath = config.basePackage.replaceAll("\\.", "/")
    val adaptedGenSourceFiles = (validationSources ++ modelSources ++ frameworkSources).map { gsf =>
      gsf.copy(file = config.baseFolder.resolve(packagePath).resolve(gsf.file.toString))
    }
    regenescaGenerator.generate(adaptedGenSourceFiles)
    println(
      s"Finished generating OpenApi for '${config.url}' with models='${config.models}', framework='${config.framework}'."
    )
    adaptedGenSourceFiles
  }
}

object OpenApiWriter {

  private val modelBackends = ModelBackend.byId
  private val frameworkBackends = FrameworkBackend.byId

  def apply(config: Config): OpenApiWriter = {
    val modelId = ModelBackendId.fromString(config.models)
    val frameworkId = FrameworkBackendId.fromString(config.framework)

    if (modelId == ModelBackendId.NoModel && frameworkId == FrameworkBackendId.NoFramework) {
      throw new RuntimeException("Invalid config: models=none and framework=none means nothing to generate.")
    }

    val modelBackend = modelBackends(modelId)
    val frameworkBackend = frameworkBackends(frameworkId)

    if (!frameworkBackend.supportedModelIds.contains(modelBackend.id)) {
      System.err.println(
        s"WARNING: potentially incompatible backend combination: models='${config.models}', framework='${config.framework}'. " +
          s"Framework '${frameworkBackend.id}' may not fully support model backend '${modelBackend.id}'. " +
          s"Generated sources may require manual import/type adjustments; prefer compatible model/framework combinations when possible."
      )
    }

    val validationId = ValidationBackendId.fromString(config.validation)
    val validationBackend = ValidationBackend.byId(validationId)

    if (!validationBackend.supportedModelIds.contains(modelId)) {
      throw new RuntimeException(
        s"Incompatible config: --models ${config.models} does not support --validation ${config.validation}. " +
          s"Validation '${validationId}' is only compatible with model backends: ${validationBackend.supportedModelIds.mkString(", ")}"
      )
    }

    new OpenApiWriter(config, modelBackend, frameworkBackend, validationBackend)
  }

  case class Config(
      url: String,
      baseFolder: Path,
      basePackage: String,
      models: String,
      framework: String,
      validation: String = "none"
  )

}
