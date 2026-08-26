package ba.sake.openapi4s

import scala.meta.{Term, Type}

object ScalaIdents {

  def termName(raw: String): Term.Name = Term.Name(raw)

  def typeName(raw: String): Type.Name = Type.Name(raw)
}
