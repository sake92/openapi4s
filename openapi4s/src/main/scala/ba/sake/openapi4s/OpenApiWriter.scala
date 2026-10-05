package ba.sake.openapi4s

import java.nio.file.Path
import ba.sake.regenesca.{GeneratedFileSource, RegenescaGenerator, SourceMerger}
import scala.meta.contrib.implicits.TreeExtensions
import scala.meta.dialects.Scala34

class OpenApiWriter(
    config: OpenApiWriter.Config,
    modelBackend: ModelBackend,
    frameworkBackend: FrameworkBackend,
    validationBackend: ValidationBackend,
    clientBackend: ClientBackend
) {
  private val openapiDefinition = OpenApiDefinition.parse(config.url)
  private val merger = SourceMerger(mergeDefBodies = true)
  private val regenescaGenerator = RegenescaGenerator(merger)

  def write(): Seq[GeneratedFileSource] = {
    println(
      s"Started generating OpenApi for '${config.url}' with models='${config.models}', framework='${config.framework}', validation='${config.validation}', client='${config.client}' ..."
    )
    val (validationSources, validationTypeMap) = validationBackend.generate(config, openapiDefinition)
    val modelSources = modelBackend.generator(config, openapiDefinition, validationTypeMap).generate()
    val modelContract = modelBackend.contract(config)
    val serverDefinition = OpenApiWriter.filterByTags(
      openapiDefinition,
      config.serverTags.orElse(config.tags),
      config.serverExcludeTags.orElse(config.excludeTags)
    )
    val frameworkSources = frameworkBackend.generator(config, serverDefinition, modelContract).generate()
    val clientDefinition = OpenApiWriter.filterByTags(
      openapiDefinition,
      config.clientTags.orElse(config.tags),
      config.clientExcludeTags.orElse(config.excludeTags)
    )
    val clientSources = clientBackend.generator(config, clientDefinition, modelContract).generate()
    val packagePath = config.basePackage.replaceAll("\\.", "/")
    val adaptedGenSourceFiles = (validationSources ++ modelSources ++ frameworkSources ++ clientSources).map { gsf =>
      gsf.copy(file = config.baseFolder.resolve(packagePath).resolve(gsf.file.toString))
    }
    regenescaGenerator.generate(adaptedGenSourceFiles)
    println(
      s"Finished generating OpenApi for '${config.url}' with models='${config.models}', framework='${config.framework}', client='${config.client}'."
    )
    adaptedGenSourceFiles
  }
}

object OpenApiWriter {

  private val modelBackends = ModelBackend.byId
  private val frameworkBackends = FrameworkBackend.byId
  private val clientBackends = ClientBackend.byId

  def apply(config: Config): OpenApiWriter = {
    val modelId = ModelBackendId.fromString(config.models)
    val frameworkId = FrameworkBackendId.fromString(config.framework)
    val clientId = ClientBackendId.fromString(config.client)

    if (
      modelId == ModelBackendId.NoModel && frameworkId == FrameworkBackendId.NoFramework && clientId == ClientBackendId.NoClient
    ) {
      throw new RuntimeException(
        "Invalid config: models=none, framework=none and client=none means nothing to generate."
      )
    }

    if (
      OpenApiDefinition.isJsonSchemaInput(
        config.url
      ) && (frameworkId != FrameworkBackendId.NoFramework || clientId != ClientBackendId.NoClient)
    ) {
      throw new RuntimeException(
        "JSON Schema input supports model generation only; omit --framework and --client."
      )
    }

    val modelBackend = modelBackends(modelId)
    val frameworkBackend = frameworkBackends(frameworkId)
    val clientBackend = clientBackends(clientId)

    if (!frameworkBackend.supportedModelIds.contains(modelBackend.id)) {
      System.err.println(
        s"WARNING: potentially incompatible backend combination: models='${config.models}', framework='${config.framework}'. " +
          s"Framework '${frameworkBackend.id}' may not fully support model backend '${modelBackend.id}'. " +
          s"Generated sources may require manual import/type adjustments; prefer compatible model/framework combinations when possible."
      )
    }

    if (!clientBackend.supportedModelIds.contains(modelBackend.id)) {
      System.err.println(
        s"WARNING: potentially incompatible backend combination: models='${config.models}', client='${config.client}'. " +
          s"Client '${clientBackend.id}' may not fully support model backend '${modelBackend.id}'. " +
          s"Generated sources may require manual import/type adjustments; prefer compatible model/client combinations when possible."
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

    new OpenApiWriter(config, modelBackend, frameworkBackend, validationBackend, clientBackend)
  }

  case class Config(
      url: String,
      baseFolder: Path,
      basePackage: String,
      models: String,
      framework: String,
      validation: String = "none",
      client: String = "none",
      tags: Option[List[String]] = None,
      excludeTags: Option[List[String]] = None,
      serverTags: Option[List[String]] = None,
      serverExcludeTags: Option[List[String]] = None,
      clientTags: Option[List[String]] = None,
      clientExcludeTags: Option[List[String]] = None
  )

  /** Keeps only path definitions whose tag is in `include` (if set) and not in `exclude` (exclude wins). Case-insensitive. */
  private[openapi4s] def filterByTags(
      definition: OpenApiDefinition,
      include: Option[List[String]],
      exclude: Option[List[String]]
  ): OpenApiDefinition =
    if (include.isEmpty && exclude.isEmpty) definition
    else {
      def matches(tags: List[String], tag: String) = tags.exists(_.equalsIgnoreCase(tag))
      definition.copy(
        pathDefinitions = PathDefinitions(
          definition.pathDefinitions.defs.filter { d =>
            !exclude.exists(matches(_, d.getTag)) && include.forall(matches(_, d.getTag))
          }
        )
      )
    }

}
