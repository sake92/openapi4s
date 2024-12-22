package ba.sake.openapi4s

import scala.jdk.CollectionConverters._
import io.swagger.parser.OpenAPIParser
import ba.sake.openapi4s.SchemaDefinition._

class OpenApiDefinitionSuite extends munit.FunSuite {

  test("parse should parse petstore.json") {
    val openapiFileUrl = getClass.getClassLoader.getResource("petstore.json")
    val openApiDefinition = OpenApiDefinition.parse(openapiFileUrl.toString)
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
    val findByStatusPath = openApiDefinition.pathDefinitions.defs.find(_.operationId == "findPetsByStatus").get
    assertEquals(
      findByStatusPath,
      PathDefinition(
        method = "GET",
        path = "/pet/findByStatus",
        pathSegments = Seq(PathSegment.Literal("pet"), PathSegment.Literal("findByStatus")),
        queryParams = List(
          QueryParam(
            name = "status",
            schema = Enum(List("available", "pending", "sold"), default = Some("available")),
            required = false
          )
        ),
        reqBody = None,
        resBody = Some(ResBodyDefinition(Arr(Ref("Pet"), None, None, false))),
        tags = List("pet"),
        summary = "Finds Pets by status",
        description = "Multiple status values can be provided with comma separated strings",
        operationId = "findPetsByStatus"
      )
    )
  }

  // https://github.com/OAI/OpenAPI-Specification/blob/3.1.1/examples/v3.0/petstore.json
  test("parse should parse petstore_3.0.0.json") {
    val openapiFileUrl = getClass.getClassLoader.getResource("petstore_3.0.0.json")
    val openApiDefinition = OpenApiDefinition.parse(openapiFileUrl.toString)
    pprint.pprintln(openApiDefinition)
    val listPetsPath = openApiDefinition.pathDefinitions.defs.find(_.operationId == "listPets").get
    assertEquals(
      listPetsPath,
      PathDefinition(
        method = "GET",
        path = "/pets",
        pathSegments = Seq(PathSegment.Literal(value = "pets")),
        queryParams = List(
          QueryParam(
            name = "limit",
            schema = Int32(default = None, maximum = Some(value = 100), minimum = None),
            required = false
          )
        ),
        reqBody = None,
        resBody = Some(value = ResBodyDefinition(schema = Ref(name = "Pets"))),
        tags = List("pets"),
        summary = "List all pets",
        description = "",
        operationId = "listPets"
      )
    )
  }

}
