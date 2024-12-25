package ba.sake.openapi4s

object exceptions {
  class UnsupportedSchemaDefinitionException(msg: String) extends RuntimeException(msg)

  class UnsupportedSchemaException(message: String) extends RuntimeException(message)
}
