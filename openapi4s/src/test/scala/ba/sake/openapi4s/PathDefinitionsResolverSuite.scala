package ba.sake.openapi4s

import scala.jdk.CollectionConverters._
import io.swagger.parser.OpenAPIParser
import ba.sake.openapi4s.SchemaDefinition._

class PathDefinitionsResolverSuite extends munit.FunSuite {

  test("PathDefinitionsResolver should resolve petstore.json path definitions") {

    val openApiDefinition = OpenApiDefinition.parse(TestUtils.getResourceUrl( "petstore.json"))
    val pathDefinitions = openApiDefinition.pathDefinitions

    // pprint.pprintln(pathDefinitions)
    assertEquals(
      pathDefinitions.defs.map(d => (d.method, d.path)),
      Seq(
        ("PUT", "/pet"),
        ("POST", "/pet"),
        ("GET", "/pet/findByStatus"),
        ("GET", "/pet/findByTags"),
        ("GET", "/pet/{petId}"),
        ("POST", "/pet/{petId}"),
        ("DELETE", "/pet/{petId}"),
        ("POST", "/pet/{petId}/uploadImage"),
        ("GET", "/store/inventory"),
        ("POST", "/store/order"),
        ("GET", "/store/order/{orderId}"),
        ("DELETE", "/store/order/{orderId}"),
        ("POST", "/user"),
        ("POST", "/user/createWithList"),
        ("GET", "/user/login"),
        ("GET", "/user/logout"),
        ("GET", "/user/{username}"),
        ("PUT", "/user/{username}"),
        ("DELETE", "/user/{username}")
      )
    )

    val findByStatusPath = pathDefinitions.defs.find(_.operationId == "findPetsByStatus").get
    assertEquals(
      findByStatusPath,
      PathDefinition(
        method = "GET",
        path = "/pet/findByStatus",
        pathSegments = List(PathSegment.Literal("pet"), PathSegment.Literal("findByStatus")),
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
  test("PathDefinitionsResolver should resolve petstore_3.0.0.json") {
    val openApiDefinition = OpenApiDefinition.parse(TestUtils.getResourceUrl( "petstore_3.0.0.json") )
    // pprint.pprintln(openApiDefinition)
    val listPetsPath = openApiDefinition.pathDefinitions.defs.find(_.operationId == "listPets").get
    assertEquals(
      listPetsPath,
      PathDefinition(
        method = "GET",
        path = "/pets",
        pathSegments = List(PathSegment.Literal(value = "pets")),
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
