package ba.sake.openapi4s

import ba.sake.openapi4s.OpenApiWriter.Config
import ba.sake.regenesca.GeneratedFileSource

sealed trait ValidationBackendId
object ValidationBackendId {
  case object None extends ValidationBackendId
  case object Iron extends ValidationBackendId
  case object Validson extends ValidationBackendId

  val all: List[ValidationBackendId] = List(None, Iron, Validson)

  def fromString(s: String): ValidationBackendId = s.toLowerCase match {
    case "none"     => None
    case "iron"     => Iron
    case "validson" => Validson
    case _ =>
      throw new RuntimeException(
        s"Unknown validation backend '${s}'. Available validation backends: 'none', 'iron', 'validson'"
      )
  }
}

trait ValidationBackend {
  def id: ValidationBackendId
  def supportedModelIds: Set[ModelBackendId]
  def generate(
      config: Config,
      openApiDefinition: OpenApiDefinition
  ): (Seq[GeneratedFileSource], Map[String, Map[String, String]])
}

object ValidationBackend {

  val none: ValidationBackend = validation.NoneValidationBackend

  val byId: Map[ValidationBackendId, ValidationBackend] = Map(
    ValidationBackendId.None -> none
  )
}
