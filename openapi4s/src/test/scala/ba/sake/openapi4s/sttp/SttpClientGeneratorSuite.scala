package ba.sake.openapi4s.sttp

import java.nio.file.Paths
import ba.sake.openapi4s._
import ba.sake.regenesca.GeneratedFileSource

class SttpClientGeneratorSuite extends munit.FunSuite {

  private val config = OpenApiWriter.Config(
    url = TestUtils.getResourceUrl("sttp_client.yaml"),
    baseFolder = Paths.get("app"),
    basePackage = "mypkg",
    models = "circe",
    framework = "none"
  )

  private val openapiDefinition = OpenApiDefinition.parse(config.url)
  private val modelContract =
    ModelBackend.byId(ModelBackendId.Circe).contract(config)

  private def generate(): Seq[GeneratedFileSource] =
    new SttpClientGenerator(config, openapiDefinition, modelContract).generate()

  test("generate() should generate one client per tag, with server constants") {
    val sources = generate()
    println("*" * 100)
    println("Generated sttp client sources for sttp_client.yaml :")
    sources.foreach { source =>
      println("*" * 50)
      println(source.file.getFileName)
      println(source.source.syntax)
    }
    val fileNames = sources.map(_.file.toString)
    assertEquals(
      fileNames.sorted,
      List("clients/DefaultClient.scala", "clients/PetClient.scala", "clients/StoreClient.scala")
    )

    val petClientSrc = sources.find(_.file.toString == "clients/PetClient.scala").get.source.syntax
    assert(petClientSrc.contains("object PetClient {"))
    assert(petClientSrc.contains("val server1: String = \"http://petstore.swagger.io/v1\""))
    assert(petClientSrc.contains("val server2: String = \"http://petstore.swagger.io/v2\""))
    assert(petClientSrc.contains("class PetClient(baseUrl: String) {"))
    assert(petClientSrc.contains("package mypkg.clients"))
    assert(petClientSrc.contains("import sttp.client4._"))
    assert(petClientSrc.contains("import sttp.client4.circe._"))
    assert(petClientSrc.contains("import mypkg.models._"))
  }

  test("generate() should handle path, query and header params") {
    val petClientSrc =
      generate().find(_.file.toString == "clients/PetClient.scala").get.source.syntax

    // path + required header + optional header
    assert(
      petClientSrc.contains(
        "def getPetById(petId: Long, api_key: String, trace: Option[String]): Request[Either[ResponseException[String], Pet]] = {"
      )
    )
    assert(petClientSrc.contains("uri\"$baseUrl/pet/$petId\""))
    assert(petClientSrc.contains(".header(\"api_key\", api_key)"))
    assert(petClientSrc.contains("trace.fold(req)(v => req.header(\"trace\", v))"))
    assert(petClientSrc.contains(".response(asJson[Pet])"))

    // required + optional query params (enum query param falls back to String)
    assert(
      petClientSrc.contains(
        "def findPetsByStatus(status: Option[String], limit: Int): Request[Either[ResponseException[String], Seq[Pet]]] = {"
      )
    )
    assert(petClientSrc.contains("uri\"$baseUrl/pet/findByStatus?status=$status&limit=$limit\""))
    // POST ops use basicRequest.post
    assert(petClientSrc.contains("basicRequest.post(uri\"$baseUrl/pet\")"))
  }

  test("generate() should derive method name from method+path when operationId is missing") {
    val storeClientSrc =
      generate().find(_.file.toString == "clients/StoreClient.scala").get.source.syntax
    assert(storeClientSrc.contains("def postStoreOrder"))
  }

  test("generate() should group ops without tags into DefaultClient") {
    val defaultClientSrc =
      generate().find(_.file.toString == "clients/DefaultClient.scala").get.source.syntax
    assert(defaultClientSrc.contains("def healthCheck"))
    // no 2xx body -> Unit response
    assert(
      defaultClientSrc.contains(
        "def healthCheck(): Request[Either[ResponseException[String], Unit]] = {"
      )
    )
    assert(
      defaultClientSrc.contains("asString.mapWithMetadata(ResponseAs.deserializeRightCatchingExceptions(_ => ()))")
    )
  }

  test("generate() should handle JSON request/response bodies (circe)") {
    val petClientSrc =
      generate().find(_.file.toString == "clients/PetClient.scala").get.source.syntax
    assert(
      petClientSrc.contains(
        "def addPet(pet: Pet): Request[Either[ResponseException[String], Pet]] = {"
      )
    )
    assert(petClientSrc.contains("basicRequest.post(uri\"$baseUrl/pet\")"))
    assert(petClientSrc.contains(".body(asJson(pet))"))
    assert(petClientSrc.contains(".response(asJson[Pet])"))
  }

  test("generate() should generate a tupson JsonSupport helper and use it") {
    val configTupson = config.copy(models = "tupson")
    val modelContractTupson = ModelBackend.byId(ModelBackendId.Tupson).contract(configTupson)
    val sources = new SttpClientGenerator(configTupson, openapiDefinition, modelContractTupson).generate()

    val jsonSupportSrc = sources.find(_.file.toString == "clients/JsonSupport.scala").get.source.syntax
    assert(jsonSupportSrc.contains("object JsonSupport {"))
    assert(jsonSupportSrc.contains("def asJson[T: JsonRW]: ResponseAs[Either[ResponseException[String], T]]"))
    assert(jsonSupportSrc.contains("ResponseAs.deserializeRightCatchingExceptions(_.parseJson[T])"))

    val petClientSrc = sources.find(_.file.toString == "clients/PetClient.scala").get.source.syntax
    // scala.meta renders wildcard imports as `_`
    assert(petClientSrc.contains("import mypkg.clients.JsonSupport._"))
    assert(petClientSrc.contains("import ba.sake.tupson.{ given, _ }"))
    assert(!petClientSrc.contains("import sttp.client4.circe._"))
    assert(
      petClientSrc.contains(
        "def addPet(pet: Pet): Request[Either[ResponseException[String], Pet]] = {"
      )
    )
    assert(petClientSrc.contains(".body(pet.toJson).contentType(\"application/json\")"))
    assert(petClientSrc.contains(".response(JsonSupport.asJson[Pet])"))
  }
}
