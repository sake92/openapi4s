package ba.sake.openapi4s.tupson

import ba.sake.openapi4s.SchemaDefinition._
import ba.sake.openapi4s.exceptions.UnsupportedSchemaException

class TupsonModelDefaultsSuite extends munit.FunSuite {
  test("integer defaults accept decimal and exponent notation exactly") {
    assertEquals(TupsonModelDefaults.optionalDefault(Opt(Int32(Some("1.0"), None, None))).get.syntax, "Some(1)")
    assertEquals(TupsonModelDefaults.optionalDefault(Opt(Int64(Some("1e3"), None, None))).get.syntax, "Some(1000L)")
    assertEquals(
      TupsonModelDefaults.optionalDefault(Opt(Int64(Some("9223372036854775807.0"), None, None))).get.syntax,
      "Some(9223372036854775807L)"
    )
  }

  test("fractional, malformed and out-of-range defaults report controlled errors") {
    val schemas = List(
      Int32(Some("1.5"), None, None),
      Int32(Some("2147483648"), None, None),
      Int32(Some("-2147483649"), None, None),
      Int64(Some("9223372036854775808"), None, None),
      Int64(Some("-9223372036854775809"), None, None),
      Int64(Some("nope"), None, None)
    )
    schemas.foreach { schema =>
      intercept[UnsupportedSchemaException] { TupsonModelDefaults.optionalDefault(Opt(schema)) }
    }
  }
}
