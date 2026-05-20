package ba.sake.openapi4s

import scala.meta._

case class ModelImportContract(
    modelFileImports: List[Import],
    frameworkImportsById: Map[String, List[Import]] = Map.empty
) {
  def frameworkImports(frameworkId: String): List[Import] =
    frameworkImportsById.getOrElse(frameworkId, List.empty)
}

object ModelImportContracts {
  val circe: ModelImportContract = ModelImportContract(
    modelFileImports = List(
      q"import java.time.*",
      q"import java.util.UUID",
      q"import io.circe.{Codec, Json}",
      q"import io.circe.derivation.{Configuration, ConfiguredCodec, ConfiguredEnumCodec}"
    ),
    frameworkImportsById = Map(
      "http4s" -> List(q"import org.http4s.circe.CirceEntityCodec.*")
    )
  )

  val tupson: ModelImportContract = ModelImportContract(
    modelFileImports = List(
      q"import java.time.*",
      q"import java.util.UUID",
      q"import org.typelevel.jawn.ast.JValue",
      q"import ba.sake.tupson.*",
      q"import ba.sake.validson.Validator"
    ),
    frameworkImportsById = Map(
      "sharaf" -> List.empty
    )
  )

  val external: ModelImportContract = ModelImportContract(
    modelFileImports = List.empty,
    frameworkImportsById = Map.empty
  )
}
