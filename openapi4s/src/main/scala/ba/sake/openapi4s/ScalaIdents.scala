package ba.sake.openapi4s

import scala.meta.{Term, Type}

object ScalaIdents {

  private val identRegex = "[a-zA-Z_][a-zA-Z0-9_]*".r

  // words that cannot be used as plain Scala 3 identifiers
  private val keywords = Set(
    "abstract", "case", "catch", "class", "def", "do", "else", "enum", "export",
    "extends", "false", "final", "finally", "for", "forSome", "given", "if",
    "implicit", "import", "lazy", "match", "new", "null", "object", "override",
    "package", "private", "protected", "return", "sealed", "super", "then",
    "this", "throw", "trait", "true", "try", "type", "val", "var", "while",
    "with", "yield"
  )

  /** Returns true if raw can be used as a plain Scala 3 identifier. */
  def isValid(raw: String): Boolean =
    raw.nonEmpty && raw != "_" && identRegex.matches(raw) && !keywords.contains(raw)

  /** Backticks raw when it is not a valid plain identifier. The backticks are syntax-only: the
    * identifier string (and thus derived JSON wire keys) stays unchanged.
    */
  def sanitize(raw: String): String =
    if (isValid(raw)) raw else s"`${raw}`"

  // members of Any/AnyRef/Product that cannot be overridden by a case class param or enum case
  private val reservedTermNames = Set(
    "toString",
    "hashCode",
    "productArity",
    "productPrefix",
    "productIterator",
    "wait",
    "notify",
    "clone",
    "finalize"
  )

  def termName(raw: String): Term.Name = {
    val sanitized = if (reservedTermNames(raw)) {
      println(s"Identifier '${raw}' is reserved in Scala, renaming it to '${raw}_'")
      s"${raw}_"
    } else raw
    Term.Name(sanitized)
  }

  def typeName(raw: String): Type.Name = Type.Name(raw)
}
