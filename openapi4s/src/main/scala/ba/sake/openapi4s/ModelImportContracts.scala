package ba.sake.openapi4s

import scala.meta._
import scala.meta.dialects.Scala34

case class ModelImportContract(
    frameworkImportsById: Map[FrameworkBackendId, List[Import]] = Map.empty
) {
  def frameworkImports(frameworkId: FrameworkBackendId): List[Import] =
    frameworkImportsById.getOrElse(frameworkId, List.empty)
}

object ModelImportContracts {
  val none = ModelImportContract(
    frameworkImportsById = Map.empty.withDefault(_ => List.empty)
  )

  val circe: ModelImportContract = ModelImportContract(
    frameworkImportsById = Map(
      FrameworkBackendId.Http4s -> List(q"import org.http4s.circe.CirceEntityCodec.*")
    )
  )

  val tupson: ModelImportContract = ModelImportContract(
    frameworkImportsById = Map(
      FrameworkBackendId.Sharaf -> List.empty
    )
  )

}
