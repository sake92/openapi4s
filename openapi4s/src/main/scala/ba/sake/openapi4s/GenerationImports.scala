package ba.sake.openapi4s

import scala.meta._
import scala.meta.dialects.Scala34

object GenerationImports {
  def modelWildcardImport(basePackage: String): Import = {
    val importer = s"${basePackage}.models.*".parse[Importer].get
    q"import ..${List(importer)}"
  }
}
