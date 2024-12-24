package ba.sake.openapi4s

import ba.sake.openapi4s.SchemaDefinition.{Bool, Named, Obj, OneOf, Opt, Ref}

class OpenApiDefinitionSuite extends munit.FunSuite {

  test("normalized should sort named schemas so that oneOf come first") {
    locally {
      val openapi = OpenApiDefinition(
        NamedSchemaDefinitions(
          Seq(
            Named("Dog", Obj(List(SchemaProperty("bark", Opt(Bool(None)))))),
            Named("Pet", OneOf(List(Ref("Cat"), Ref("Dog")), "pet_type")),
            Named("Cat", Obj(List(SchemaProperty("hunts", Opt(Bool(None))))))
          )
        ),
        PathDefinitions(defs = List())
      )
      assertEquals(openapi.normalized.namedSchemaDefinitions.defs.head.name, "Pet")
    }
    locally {
      val openapi = OpenApiDefinition(
        NamedSchemaDefinitions(
          Seq(
            Named("Dog", Obj(List(SchemaProperty("bark", Opt(Bool(None)))))),
            Named("Pet2", OneOf(List(Ref("Cat"), Ref("Dog")), "pet_type")),
            Named("Cat", Obj(List(SchemaProperty("hunts", Opt(Bool(None)))))),
            Named("Pet", OneOf(List(Ref("Cat"), Ref("Dog")), "pet_type"))
          )
        ),
        PathDefinitions(defs = List())
      )
      assertEquals(openapi.normalized.namedSchemaDefinitions.defs.head.name, "Pet")
      assertEquals(openapi.normalized.namedSchemaDefinitions.defs.tail.head.name, "Pet2")
    }
  }

}
