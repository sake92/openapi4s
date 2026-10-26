package ba.sake.openapi4s

import com.sun.net.httpserver.{HttpHandler, HttpServer}

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets

class JsonSchemaUrlSuite extends munit.FunSuite {

  private def jsonHandler(body: String): HttpHandler = { exchange =>
    val bytes = body.getBytes(StandardCharsets.UTF_8)
    exchange.getResponseHeaders.set("Content-Type", "application/json; charset=utf-8")
    exchange.sendResponseHeaders(200, bytes.length.toLong)
    val output = exchange.getResponseBody
    try output.write(bytes)
    finally output.close()
  }

  private def withServer(routes: String => Map[String, String])(test: String => Unit): Unit = {
    val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    val baseUrl = s"http://127.0.0.1:${server.getAddress.getPort}"
    routes(baseUrl).foreach { case (path, body) =>
      server.createContext(path, jsonHandler(body))
    }
    server.start()
    try test(baseUrl)
    finally server.stop(0)
  }

  test("parses a JSON Schema from an HTTP URL and promotes $defs") {
    val schema = """{
      "$schema":"https://json-schema.org/draft/2020-12/schema",
      "title":"Person",
      "type":"object",
      "required":["name","status"],
      "properties":{
        "name":{"type":"string"},
        "status":{"$ref":"#/$defs/Status"}
      },
      "$defs":{
        "Status":{"type":"string","enum":["active","inactive"]}
      }
    }"""
    withServer(_ => Map("/person.json" -> schema)) { baseUrl =>
      val definition = OpenApiDefinition.parse(s"$baseUrl/person.json")

      assertEquals(definition.pathDefinitions.defs, List.empty)
      assertEquals(definition.namedSchemaDefinitions.defs.map(_.name).toSet, Set("Person", "Status"))
    }
  }

  test("uses the URL file name when a remote schema has no title") {
    val schema = """{
      "type":"object",
      "required":["name"],
      "properties":{"name":{"type":"string"}}
    }"""
    withServer(_ => Map("/no_title.json" -> schema)) { baseUrl =>
      val definition = OpenApiDefinition.parse(s"$baseUrl/no_title.json")

      assertEquals(definition.namedSchemaDefinitions.defs.map(_.name), Seq("no_title"))
    }
  }

  test("resolves relative references between remote JSON Schema documents") {
    val address = """{
      "title":"Address",
      "type":"object",
      "required":["street"],
      "properties":{"street":{"type":"string"}}
    }"""
    val customer = """{
      "title":"Customer",
      "type":"object",
      "required":["address"],
      "properties":{"address":{"$ref":"address.json"}}
    }"""
    withServer(_ => Map("/address.json" -> address, "/customer.json" -> customer)) { baseUrl =>
      val definition = OpenApiDefinition.parse(s"$baseUrl/customer.json")
      val customerSchema =
        definition.namedSchemaDefinitions.defs.find(_.name == "Customer").getOrElse(fail("Customer missing"))

      assertEquals(definition.namedSchemaDefinitions.defs.map(_.name).toSet, Set("Address", "Customer"))
      assert(customerSchema.schema.toString.contains("Ref(Address)"))
    }
  }

  test("resolves absolute URL references from a remote JSON Schema") {
    val address = """{
      "title":"Address",
      "type":"object",
      "required":["street"],
      "properties":{"street":{"type":"string"}}
    }"""
    withServer { baseUrl =>
      val customer = s"""{
        "title":"Customer",
        "type":"object",
        "required":["address"],
        "properties":{"address":{"$$ref":"$baseUrl/address.json"}}
      }"""
      Map("/address.json" -> address, "/customer.json" -> customer)
    } { baseUrl =>
      val definition = OpenApiDefinition.parse(s"$baseUrl/customer.json")
      val customerSchema =
        definition.namedSchemaDefinitions.defs.find(_.name == "Customer").getOrElse(fail("Customer missing"))

      assertEquals(definition.namedSchemaDefinitions.defs.map(_.name).toSet, Set("Address", "Customer"))
      assert(customerSchema.schema.toString.contains("Ref(Address)"))
    }
  }

  test("still parses OpenAPI documents served over HTTP") {
    val openapi = """{
      "openapi":"3.0.0",
      "info":{"title":"Remote API","version":"1.0.0"},
      "paths":{},
      "components":{
        "schemas":{
          "Pet":{"type":"object","required":["name"],"properties":{"name":{"type":"string"}}}
        }
      }
    }"""
    withServer(_ => Map("/openapi.json" -> openapi)) { baseUrl =>
      val definition = OpenApiDefinition.parse(s"$baseUrl/openapi.json")

      assertEquals(definition.namedSchemaDefinitions.defs.map(_.name), Seq("Pet"))
    }
  }

  test("reports HTTP failures for remote JSON Schema URLs") {
    withServer(_ => Map.empty) { baseUrl =>
      val error = intercept[RuntimeException] {
        OpenApiDefinition.parse(s"$baseUrl/missing.json")
      }

      assert(error.getMessage.contains("HTTP 404"), error.getMessage)
    }
  }
}
