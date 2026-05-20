package ba.sake.openapi4s

import scala.meta._

object GenerationImports {
  def modelWildcardImport(basePackage: String): Import = {
    val importer = s"${basePackage}.models.*".parse[Importer].get
    q"import ..${List(importer)}"
  }
}
