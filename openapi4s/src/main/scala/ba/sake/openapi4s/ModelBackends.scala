package ba.sake.openapi4s

import java.nio.file.Paths
import scala.meta._
import ba.sake.openapi4s.OpenApiGenerator._
import ba.sake.openapi4s.circe.CirceModelGenerator
import ba.sake.openapi4s.tupson.TupsonModelGenerator
import ba.sake.regenesca.GeneratedFileSource

object ModelBackends {
  private def generatePkgSelect(pkg: String): Term.Ref = {
    pkg
      .split("\\.")
      .map(Term.Name)
      .reduceLeft[Term.Ref](Term.Select(_, _))
  }

  val circe: ModelBackend = new ModelBackend {
    override val id: String = "circe"
    override val flavor: ModelFlavor = ModelFlavor.Circe
    override val imports: ModelImportContract = ModelImportContracts.circe
    override def generateSources(config: Config, openapiDefinition: OpenApiDefinition): Seq[GeneratedFileSource] = {
      val modelsPkg = generatePkgSelect(s"${config.basePackage}.models")
      val modelGenerator = new CirceModelGenerator(openapiDefinition)
      openapiDefinition.namedSchemaDefinitions.defs.flatMap { namedSchemaDef =>
        val namedSchemaName = namedSchemaDef.name.capitalize
        val modelSources = modelGenerator.generateModelSources(namedSchemaDef, None)
        val allStmts = imports.modelFileImports ++ modelSources
        Option.when(modelSources.nonEmpty) {
          GeneratedFileSource(
            Paths.get(s"models/${namedSchemaName}.scala"),
            source""" package ${modelsPkg} { ..${allStmts} } """
          )
        }
      }
    }
  }

  val tupson: ModelBackend = new ModelBackend {
    override val id: String = "tupson"
    override val flavor: ModelFlavor = ModelFlavor.Tupson
    override val imports: ModelImportContract = ModelImportContracts.tupson
    override def generateSources(config: Config, openapiDefinition: OpenApiDefinition): Seq[GeneratedFileSource] = {
      val modelsPkg = generatePkgSelect(s"${config.basePackage}.models")
      val modelGenerator = new TupsonModelGenerator(openapiDefinition)
      openapiDefinition.namedSchemaDefinitions.defs.flatMap { namedSchemaDef =>
        val namedSchemaName = namedSchemaDef.name.capitalize
        val modelSources = modelGenerator.generateModelSources(namedSchemaDef, None)
        val allStmts = imports.modelFileImports ++ modelSources
        Option.when(modelSources.nonEmpty) {
          GeneratedFileSource(
            Paths.get(s"models/${namedSchemaName}.scala"),
            source"""
              // generated with OpenApi4s
              package ${modelsPkg} { ..${allStmts} }
            """
          )
        }
      }
    }
  }

  val byId: Map[String, ModelBackend] = Map(
    "circe" -> circe,
    "tupson" -> tupson
  )
}
