package ba.sake.openapi4s

class ScalaIdentsSuite extends munit.FunSuite {

  test("termName keeps valid identifiers") {
    assertEquals(ScalaIdents.termName("open").syntax, "open")
    assertEquals(ScalaIdents.termName("user_id").syntax, "user_id")
  }

  test("termName backticks keywords and literals") {
    assertEquals(ScalaIdents.termName("type").syntax, "`type`")
    assertEquals(ScalaIdents.termName("true").syntax, "`true`")
  }

  test("termName backticks invalid identifiers") {
    assertEquals(ScalaIdents.termName("read-only").syntax, "`read-only`")
    assertEquals(ScalaIdents.termName("16x16").syntax, "`16x16`")
  }

  test("typeName backticks dashed schema names") {
    assertEquals(ScalaIdents.typeName("Webhook-config-insecure-ssl").syntax, "`Webhook-config-insecure-ssl`")
    assertEquals(ScalaIdents.typeName("Pet").syntax, "Pet")
  }
}
