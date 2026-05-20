package ba.sake.openapi4s

import java.nio.file.Path
import ba.sake.openapi4s.http4s.Http4sGenerator
import ba.sake.openapi4s.sharaf.SharafGenerator
import ba.sake.regenesca.{GeneratedFileSource, RegenescaGenerator, SourceMerger}

trait OpenApiGenerator {
  def generate(): Unit
}

object OpenApiGenerator {

  sealed trait ModelFlavor
  object ModelFlavor {
    case object Circe extends ModelFlavor
    case object Tupson extends ModelFlavor
    case object External extends ModelFlavor
  }

  case class ModelContract(
      packageName: String,
      flavor: ModelFlavor
  )

  trait ModelBackend {
    def id: String
    def flavor: ModelFlavor
    def generateSources(config: Config, openapiDefinition: OpenApiDefinition): Seq[GeneratedFileSource]
    def contract(config: Config): ModelContract = ModelContract(s"${config.basePackage}.models", flavor)
  }

  trait FrameworkBackend {
    def id: String
    def requiredModelFlavor: ModelFlavor
    def generateSources(
        config: Config,
        openapiDefinition: OpenApiDefinition,
        modelContract: ModelContract
    ): Seq[GeneratedFileSource]
  }

  private object CirceModelBackend extends ModelBackend {
    override val id: String = "circe"
    override val flavor: ModelFlavor = ModelFlavor.Circe
    override def generateSources(config: Config, openapiDefinition: OpenApiDefinition): Seq[GeneratedFileSource] = {
      new Http4sGenerator(config, openapiDefinition).generateSources.filter(_.file.toString.startsWith("models/"))
    }
  }

  private object TupsonModelBackend extends ModelBackend {
    override val id: String = "tupson"
    override val flavor: ModelFlavor = ModelFlavor.Tupson
    override def generateSources(config: Config, openapiDefinition: OpenApiDefinition): Seq[GeneratedFileSource] = {
      new SharafGenerator(config, openapiDefinition).generateSources.filter(_.file.toString.startsWith("models/"))
    }
  }

  private object Http4sFrameworkBackend extends FrameworkBackend {
    override val id: String = "http4s"
    override val requiredModelFlavor: ModelFlavor = ModelFlavor.Circe
    override def generateSources(
        config: Config,
        openapiDefinition: OpenApiDefinition,
        modelContract: ModelContract
    ): Seq[GeneratedFileSource] = {
      new Http4sGenerator(config, openapiDefinition).generateSources.filter(_.file.toString.startsWith("routes/"))
    }
  }

  private object SharafFrameworkBackend extends FrameworkBackend {
    override val id: String = "sharaf"
    override val requiredModelFlavor: ModelFlavor = ModelFlavor.Tupson
    override def generateSources(
        config: Config,
        openapiDefinition: OpenApiDefinition,
        modelContract: ModelContract
    ): Seq[GeneratedFileSource] = {
      new SharafGenerator(config, openapiDefinition).generateSources.filter(_.file.toString.startsWith("controllers/"))
    }
  }

  private val modelBackends: Map[String, ModelBackend] = Map(
    "circe" -> CirceModelBackend,
    "tupson" -> TupsonModelBackend
  )
  private val frameworkBackends: Map[String, FrameworkBackend] = Map(
    "http4s" -> Http4sFrameworkBackend,
    "sharaf" -> SharafFrameworkBackend
  )

  @deprecated("Use OpenApiGenerator(config) with models/framework fields", since = "0.7.0")
  def apply(name: String, config: Config): OpenApiGenerator = {
    name.toLowerCase match {
      case "http4s" => apply(config.copy(models = "circe", framework = "http4s"))
      case "sharaf" => apply(config.copy(models = "tupson", framework = "sharaf"))
      case other    => throw new RuntimeException(s"Unknown generator '${other}'. Available generators: 'http4s', 'sharaf'")
    }
  }

  def apply(config: Config): OpenApiGenerator = {
    val modelName = config.models.toLowerCase
    val frameworkName = config.framework.toLowerCase

    if (modelName == "none" && frameworkName == "none") {
      throw new RuntimeException("Invalid config: models=none and framework=none means nothing to generate.")
    }

    val modelBackendOpt = if (modelName == "none") None else modelBackends.get(modelName)
    if (modelName != "none" && modelBackendOpt.isEmpty) {
      throw new RuntimeException(
        s"Unknown models backend '${config.models}'. Available models backends: '${(modelBackends.keys.toList :+ "none").sorted.mkString("', '")}'"
      )
    }

    val frameworkBackendOpt = if (frameworkName == "none") None else frameworkBackends.get(frameworkName)
    if (frameworkName != "none" && frameworkBackendOpt.isEmpty) {
      throw new RuntimeException(
        s"Unknown framework backend '${config.framework}'. Available framework backends: '${(frameworkBackends.keys.toList :+ "none").sorted.mkString("', '")}'"
      )
    }

    val modelFlavor = modelBackendOpt.map(_.flavor).getOrElse(ModelFlavor.External)
    frameworkBackendOpt.foreach { frameworkBackend =>
      if (modelFlavor != ModelFlavor.External && frameworkBackend.requiredModelFlavor != modelFlavor) {
        println(
          s"WARNING: potentially incompatible backend combination: models='${config.models}', framework='${config.framework}'. " +
            s"Framework '${frameworkBackend.id}' typically expects models flavor '${frameworkBackend.requiredModelFlavor.toString.toLowerCase}'."
        )
      }
      if (modelFlavor == ModelFlavor.External) {
        println(
          s"WARNING: models=none with framework='${frameworkBackend.id}'. " +
            s"Generation will reference ${config.basePackage}.models types that must already exist."
        )
      }
    }

    new ComposedOpenApiGenerator(config, modelBackendOpt, frameworkBackendOpt)
  }

  case class Config(
      url: String,
      baseFolder: Path,
      basePackage: String,
      models: String = "tupson",
      framework: String = "sharaf"
  )

  private class ComposedOpenApiGenerator(
      config: Config,
      modelBackendOpt: Option[ModelBackend],
      frameworkBackendOpt: Option[FrameworkBackend]
  ) extends OpenApiGenerator {
    private val openapiDefinition = OpenApiDefinition.parse(config.url)
    private val merger = SourceMerger(mergeDefBodies = true)
    private val regenescaGenerator = RegenescaGenerator(merger)

    override def generate(): Unit = {
      println(
        s"Started generating OpenApi for '${config.url}' with models='${config.models}', framework='${config.framework}' into '${config.baseFolder}' ..."
      )
      val modelSources = modelBackendOpt.toList.flatMap(_.generateSources(config, openapiDefinition))
      val modelContract = modelBackendOpt.map(_.contract(config)).getOrElse(ModelContract(s"${config.basePackage}.models", ModelFlavor.External))
      val frameworkSources =
        frameworkBackendOpt.toList.flatMap(_.generateSources(config, openapiDefinition, modelContract))
      val packagePath = config.basePackage.replaceAll("\\.", "/")
      val adaptedGenSourceFiles = (modelSources ++ frameworkSources).map { gsf =>
        gsf.copy(file = config.baseFolder.resolve(packagePath).resolve(gsf.file.toString))
      }
      regenescaGenerator.generate(adaptedGenSourceFiles)
      println(
        s"Finished generating OpenApi for '${config.url}' with models='${config.models}', framework='${config.framework}'."
      )
    }
  }

}
