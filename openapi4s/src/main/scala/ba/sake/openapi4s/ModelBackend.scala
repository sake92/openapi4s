package ba.sake.openapi4s

import scala.meta._
import scala.meta.dialects.Scala34
import ba.sake.openapi4s.OpenApiWriter.Config
import ba.sake.openapi4s.circe.CirceModelGenerator
import ba.sake.openapi4s.tupson.TupsonModelGenerator
import ba.sake.regenesca.GeneratedFileSource

sealed trait ModelBackendId
object ModelBackendId {
  case object Circe extends ModelBackendId
  case object Tupson extends ModelBackendId
  case object NoModel extends ModelBackendId

  val all: List[ModelBackendId] = List(Circe, Tupson, NoModel)

  def fromString(s: String): ModelBackendId = s.toLowerCase match {
    case "circe"  => Circe
    case "tupson" => Tupson
    case "none"   => NoModel
    case _ =>
      throw new RuntimeException(
        s"Unknown model backend '${s}'. Available model backends: 'circe', 'tupson', 'none'"
      )
  }
}

case class ModelContract(
    packageName: String,
    id: ModelBackendId,
    imports: ModelImportContract
)

trait ModelBackend {
  def id: ModelBackendId
  def imports: ModelImportContract
  def generator(config: Config, openapiDefinition: OpenApiDefinition): OpenApiGenerator
  def contract(config: Config): ModelContract = ModelContract(s"${config.basePackage}.models", id, imports)
}

object ModelBackend {

  private def generatePkgSelect(pkg: String): Term.Ref = {
    pkg
      .split("\\.")
      .map(Term.Name(_))
      .reduceLeft[Term.Ref](Term.Select(_, _))
  }

  val none: ModelBackend = new ModelBackend {
    override val id: ModelBackendId = ModelBackendId.NoModel
    override val imports: ModelImportContract = ModelImportContracts.none
    override def generator(config: Config, openapiDefinition: OpenApiDefinition): OpenApiGenerator =
      new OpenApiGenerator {
        override def generate(): Seq[GeneratedFileSource] = Seq.empty
      }
  }

  val circe: ModelBackend = new ModelBackend {
    override val id: ModelBackendId = ModelBackendId.Circe
    override val imports: ModelImportContract = ModelImportContracts.circe
    override def generator(config: Config, openapiDefinition: OpenApiDefinition): OpenApiGenerator =
      new CirceModelGenerator(config, openapiDefinition)

  }

  val tupson: ModelBackend = new ModelBackend {
    override val id: ModelBackendId = ModelBackendId.Tupson
    override val imports: ModelImportContract = ModelImportContracts.tupson
    override def generator(config: Config, openapiDefinition: OpenApiDefinition): OpenApiGenerator =
      new TupsonModelGenerator(config, openapiDefinition)
  }

  val byId: Map[ModelBackendId, ModelBackend] = Map(
    ModelBackendId.NoModel -> none,
    ModelBackendId.Circe -> circe,
    ModelBackendId.Tupson -> tupson
  )
}
