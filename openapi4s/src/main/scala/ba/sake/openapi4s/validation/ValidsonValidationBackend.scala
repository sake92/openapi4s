package ba.sake.openapi4s
package validation

import ba.sake.openapi4s.OpenApiWriter.Config
import ba.sake.regenesca.GeneratedFileSource

object ValidsonValidationBackend extends ValidationBackend {
  override val id: ValidationBackendId = ValidationBackendId.Validson
  override val supportedModelIds: Set[ModelBackendId] = Set(ModelBackendId.Tupson)

  override def generate(
      config: Config,
      openApiDefinition: OpenApiDefinition
  ): (Seq[GeneratedFileSource], Map[String, Map[String, String]]) =
    // Validson is integrated directly into TupsonModelGenerator via ValidsonUtils.
    // No separate file generation needed.
    (Seq.empty, Map.empty)
}
