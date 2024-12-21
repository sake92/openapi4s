package ba.sake.openapi4s

import scala.jdk.CollectionConverters._
import io.swagger.parser.OpenAPIParser

class PathsResolverSuite extends munit.FunSuite {

  test("resolveNamedSchemas should resolve named schemas") {
    val openapi = new OpenAPIParser().readLocation("./petstore.json", null, null).getOpenAPI
    val schemaDefinitionResolver = new SchemaDefinitionResolver()
    val pathsResolver = new PathsResolver(schemaDefinitionResolver)
    val pathDefinitions = pathsResolver.resolve(openapi.getPaths)
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
  }

}
