package ba.sake.openapi4s

import scala.jdk.CollectionConverters._
import io.swagger.parser.OpenAPIParser

class SchemaDefinitionResolverSuite extends munit.FunSuite {

  test("resolveNamedSchemas should resolve named schemas") {
    val openapi = new OpenAPIParser().readLocation("./petstore.json", null, null).getOpenAPI
    val schemaDefinitionResolver = new SchemaDefinitionResolver()
    val namedSchemaDefinitions = schemaDefinitionResolver.resolveNamedSchemas(
      openapi.getComponents.getSchemas.asScala.toMap
    )
    // pprint.pprintln(namedSchemaDefinitions)
    assertEquals(
      namedSchemaDefinitions.defs.map(_.name),
      Seq("Pet", "Category", "Address", "User", "Order", "ApiResponse", "Tag", "Customer")
    )
  }

}
