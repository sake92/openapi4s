import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.http4s.{Request, Status, Uri}
import org.http4s.circe.CirceEntityDecoder.*
import querytest.routes.CompletedQueryRoutes

object QueryCheck {
  def main(args: Array[String]): Unit = {
    val app = new CompletedQueryRoutes().routes.orNotFound
    def check(query: String, expected: Status, body: Option[String] = None): Unit = {
      val request = Request[IO](uri = Uri.unsafeFromString(query))
      val response = app.run(request).unsafeRunSync()
      assert(response.status == expected, s"$query: ${response.status} != $expected")
      body.foreach { expectedBody =>
        val actual = response.as[io.circe.Json].unsafeRunSync().asString
        assert(actual.contains(expectedBody), s"$query: $actual != $expectedBody")
      }
    }
    val required = "/search?limit=7&ids=11&ids=23"
    check(required, Status.Ok, Some("7|None|None|11,23||None|None|None|None|None|None|None"))
    check(
      required + "&class=true&sort-by=name&colors=red&colors=blue&status=open" +
        "&other-id=123e4567-e89b-12d3-a456-426614174000&id=123e4567-e89b-12d3-a456-426614174000&date=2026-10-24&at=2026-10-24T12:00:00Z" +
        "&ratio=1.5&score=2.75&state=active&kind=2&queryParam1=hello&req=world",
      Status.Ok,
      Some(
        "7|Some(true)|Some(name)|11,23|red,blue|Some(open)|Some(123e4567-e89b-12d3-a456-426614174000)|" +
          "Some(2026-10-24)|Some(2026-10-24T12:00:00Z)|Some(1.5)|Some(2.75)|Some(active)"
      )
    )
    check("/search?ids=11", Status.NotFound)
    check("/search?limit=7", Status.NotFound)
    check("/search?limit=oops&ids=11", Status.NotFound)
    check(required + "&ids=oops", Status.NotFound)
    List(
      "class=oops",
      "id=bad",
      "date=bad",
      "at=bad",
      "ratio=bad",
      "score=bad",
      "state=bad"
    ).foreach { invalid =>
      check(required + "&" + invalid, Status.NotFound)
    }
    check(required + "&status=custom&colors=green&kind=3", Status.Ok)
    check("/other", Status.NotImplemented)
    check("/other?limit=text", Status.NotImplemented)
    check("/again?limit=text", Status.NotImplemented)
    check("/health", Status.NotImplemented)
    check("/collision/path?limit=9", Status.NotImplemented)
    println("http4s query checks passed")
  }
}
