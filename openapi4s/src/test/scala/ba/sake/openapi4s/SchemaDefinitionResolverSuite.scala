package ba.sake.openapi4s

import scala.jdk.CollectionConverters._
import io.swagger.parser.OpenAPIParser
import ba.sake.openapi4s.SchemaDefinition._

class SchemaDefinitionResolverSuite extends munit.FunSuite {

  test("resolveNamedSchemas should resolve named schemas") {
    val openApiDefinition = OpenApiDefinition.parse("petstore.json")
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

}
