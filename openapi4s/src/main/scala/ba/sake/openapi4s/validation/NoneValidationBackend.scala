package ba.sake.openapi4s
package validation

import ba.sake.openapi4s.OpenApiWriter.Config
import ba.sake.regenesca.GeneratedFileSource

object NoneValidationBackend extends ValidationBackend {
  override val id: ValidationBackendId = ValidationBackendId.None
  override val supportedModelIds: Set[ModelBackendId] =
    Set(ModelBackendId.NoModel, ModelBackendId.Circe, ModelBackendId.Tupson)

  override def generate(
      config: Config,
      openApiDefinition: OpenApiDefinition
  ): (Seq[GeneratedFileSource], Map[String, Map[String, String]]) =
    (Seq.empty, Map.empty)
}
