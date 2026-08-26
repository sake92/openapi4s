package ba.sake.openapi4s

import scala.meta.{Term, Type}

object ScalaIdents {

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
