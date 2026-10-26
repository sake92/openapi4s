import cats.effect.IO
import cats.effect.unsafe.implicits.global
import example.models.Pet
import example.routes.PetsRoutes
import io.circe.{Decoder, Encoder, Json}
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.implicits.*
import org.http4s.server.middleware.ErrorHandling

object DiscriminatorMappingCheck {
  def main(args: Array[String]): Unit = {
    val cat = Json.obj("kind" -> Json.fromString("cat"), "name" -> Json.fromString("Milo"), "lives" -> Json.fromInt(9))
    val dog = Json.obj("kind" -> Json.fromString("dog"), "name" -> Json.fromString("Rex"), "good" -> Json.True)
    val decoder = summon[Decoder[Pet]]
    val encoder = summon[Encoder[Pet]]
    val app = ErrorHandling.httpRoutes[IO](new PetsRoutes().routes).orNotFound
    for (json <- List(cat, dog)) {
      val decoded = decoder.decodeJson(json).fold(throw _, identity)
      val encoded = encoder(decoded)
      assert(encoded == json, s"Expected $json, got $encoded")
      assert(decoder.decodeJson(encoded) == Right(decoded))
      val response = app.run(Request[IO](Method.POST, uri"/pets").withEntity(json)).unsafeRunSync()
      assert(response.status == Status.Ok, response.status)
      assert(response.as[Json].unsafeRunSync() == json)
    }
    val invalid = List(
      cat.mapObject(_.add("kind", Json.fromString("Cat"))),
      cat.mapObject(_.add("kind", Json.fromString("unknown"))),
      cat.mapObject(_.remove("kind")),
      cat.mapObject(_.add("name", Json.fromString(""))),
      cat.mapObject(_.add("lives", Json.fromInt(10)))
    )
    for (json <- invalid) {
      assert(decoder.decodeJson(json).isLeft, json)
      val response = app.run(Request[IO](Method.POST, uri"/pets").withEntity(json)).unsafeRunSync()
      assert(response.status == Status.UnprocessableEntity, response.status)
    }
  }
}
