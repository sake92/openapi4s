package ba.sake.openapi4s

import scala.jdk.CollectionConverters._
import io.swagger.parser.OpenAPIParser
import ba.sake.openapi4s.SchemaDefinition._
import ba.sake.openapi4s.EnumLiteral._

class SchemaDefinitionResolverSuite extends munit.FunSuite {

  test("SchemaDefinitionResolver should resolve petstore.json named schemas") {
    val openApiDefinition = OpenApiDefinition.parse(TestUtils.getResourceUrl( "petstore.json"))
    // pprint.pprintln(openApiDefinition)
    assertEquals(
      openApiDefinition.namedSchemaDefinitions.defs.map(_.name),
      Seq("Pet", "Category", "Address", "User", "Order", "ApiResponse", "Tag", "Customer")
    )
    val petSchema = openApiDefinition.namedSchemaDefinitions.defs.find(_.name == "Pet").get
    assertEquals(
      petSchema,
      SchemaDefinition.Named(
        "Pet",
        Obj(
          List(
            SchemaProperty("id", Opt(Int64(None, None, None))),
            SchemaProperty("name", Str(None, None, None, None)),
            SchemaProperty("category", Opt(Ref("Category"))),
            SchemaProperty("photoUrls", Arr(Str(None, None, None, None), None, None, uniqueItems = false)),
            SchemaProperty("tags", Opt(Arr(Ref("Tag"), None, None, uniqueItems = false))),
            SchemaProperty("status", Opt(Enum(List("available", "pending", "sold"), None)))
          )
        )
      )
    )
  }

  test("SchemaDefinitionResolver should resolve oneOf.yaml named schemas") {
    val openApiDefinition = OpenApiDefinition.parse(TestUtils.getResourceUrl( "oneOf.yaml"))
   // pprint.pprintln(openApiDefinition)
  }

  test("SchemaDefinitionResolver should resolve tupson_features.yaml named schemas") {
    val openApiDefinition = OpenApiDefinition.parse(TestUtils.getResourceUrl("tupson_features.yaml"))
    def named(name: String) = openApiDefinition.namedSchemaDefinitions.defs.find(_.name == name).get
    // anonymous objects stay Obj
    assertEquals(
      named("Container"),
      Named(
        "Container",
        Obj(
          List(
            SchemaProperty(
              "meta",
              Obj(
                List(
                  SchemaProperty("kind", Str(None, None, None, None)),
                  SchemaProperty("age", Int32(None, None, None))
                )
              )
            ),
            SchemaProperty(
              "nested",
              Obj(
                List(
                  SchemaProperty(
                    "inner",
                    Obj(List(SchemaProperty("x", Str(None, None, None, None))))
                  )
                )
              )
            ),
            SchemaProperty(
              "items",
              Arr(
                Obj(
                  List(
                    SchemaProperty("a", Int32(None, None, None)),
                    SchemaProperty("b", Str(None, None, None, None))
                  )
                ),
                None,
                None,
                uniqueItems = false
              )
            )
          )
        )
      )
    )
    // inline string enums
    assertEquals(
      named("WithEnum"),
      Named(
        "WithEnum",
        Obj(
          List(
            SchemaProperty("status", Enum(List("available", "pending", "sold"), None)),
            SchemaProperty("singleVal", Enum(List("only"), None))
          )
        )
      )
    )
    // integer and boolean enums
    assertEquals(
      named("WithIntEnum"),
      Named("WithIntEnum", Obj(List(SchemaProperty("num", EnumLiterals(List(IntValue(1), IntValue(2)), None)))))
    )
    assertEquals(
      named("WithBoolEnum"),
      Named(
        "WithBoolEnum",
        Obj(List(SchemaProperty("flag", EnumLiterals(List(BoolValue(true), BoolValue(false)), None))))
      )
    )
    // additionalProperties
    assertEquals(
      named("WithMap"),
      Named(
        "WithMap",
        Obj(
          List(
            SchemaProperty("extra", MapObj(Some(Str(None, None, None, None)))),
            SchemaProperty("freeForm", MapObj(None))
          )
        )
      )
    )
    // oneOf with/without discriminator
    assertEquals(
      named("Pet"),
      Named("Pet", OneOf(List(Ref("Cat"), Ref("Dog")), Some("pet_type")))
    )
    assertEquals(
      named("Animal"),
      Named("Animal", OneOf(List(Ref("Cat"), Ref("Dog")), None))
    )
    // anyOf
    assertEquals(
      named("AnyThing"),
      Named("AnyThing", AnyOf(List(Ref("Cat"), Ref("Dog"))))
    )
    // named non-string enum and named map
    assertEquals(
      named("Level"),
      Named("Level", EnumLiterals(List(IntValue(1), IntValue(2), IntValue(3)), None))
    )
    assertEquals(
      named("Tags"),
      Named("Tags", MapObj(Some(Str(None, None, None, None))))
    )
  }

  test("SchemaDefinitionResolver should resolve tupson_features_31.yaml named schemas") {
    val openApiDefinition = OpenApiDefinition.parse(TestUtils.getResourceUrl("tupson_features_31.yaml"))
    def named(name: String) = openApiDefinition.namedSchemaDefinitions.defs.find(_.name == name).get
    assertEquals(
      named("WithConst"),
      Named(
        "WithConst",
        Obj(
          List(
            SchemaProperty("kind", Const(StrValue("dog"), None)),
            SchemaProperty("count", Const(IntValue(5), None))
          )
        )
      )
    )
    assertEquals(
      named("Marker"),
      Named("Marker", Const(StrValue("abc"), None))
    )
  test("resolveSchema(null) returns Unknown instead of NPE") {
    val resolver = new SchemaDefinitionResolver()
    assertEquals(resolver.resolveSchema(null, "ctx"), SchemaDefinition.Unknown())
  }

}
