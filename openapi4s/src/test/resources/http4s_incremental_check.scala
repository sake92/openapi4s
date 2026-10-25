import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.circe.Json
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.implicits.*
import org.http4s.server.middleware.ErrorHandling
import example.routes.UsersRoutes

object IncrementalCheck {
  def main(args: Array[String]): Unit = {
    val app = ErrorHandling.httpRoutes[IO](new UsersRoutes().routes).orNotFound
    val check = for {
      created <- app.run(Request[IO](Method.POST, uri"/users").withEntity(Json.obj("age" -> Json.fromInt(30))))
      _ <- IO(assert(created.status == Status.Created, created.status))
      body <- created.as[Json]
      _ <- IO(assert(body == Json.obj("age" -> Json.fromInt(30)), body))
      invalid <- app.run(Request[IO](Method.POST, uri"/users").withEntity(Json.obj("age" -> Json.fromInt(17))))
      _ <- IO(assert(invalid.status == Status.UnprocessableEntity, invalid.status))
      added <- app.run(Request[IO](Method.GET, uri"/users"))
      _ <- IO(assert(added.status == Status.NotImplemented, added.status))
      missing <- app.run(Request[IO](Method.GET, uri"/missing"))
      _ <- IO(assert(missing.status == Status.NotFound, missing.status))
    } yield ()
    check.unsafeRunSync()
  }
}
