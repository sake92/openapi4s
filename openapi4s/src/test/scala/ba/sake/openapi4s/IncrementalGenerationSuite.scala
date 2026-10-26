package ba.sake.openapi4s

import java.nio.file.{Files, Path, StandardCopyOption}
import scala.concurrent.duration._

class IncrementalGenerationSuite extends munit.FunSuite {

  override def munitTimeout = 10.minutes

  test("regeneration adds an http4s route while preserving implemented for-comprehensions") {
    val folder = Files.createTempDirectory("openapi4s-incremental-route")
    generate(folder)
    val routes = folder.resolve("example/routes/UsersRoutes.scala")
    val implemented = Files
      .readString(routes)
      .replace(
        "NotImplemented(\"TODO: return User\")",
        "Ok(\"implemented\")"
      )
    Files.writeString(routes, implemented)

    generate(folder, includeList = true)
    val updated = Files.readString(routes)
    assert(updated.contains("case req @ GET -> Root / \"users\" =>"))
    assert(updated.contains("Ok(\"implemented\")"))
    assertEquals(occurrences(updated, "resp <-"), 3)
    generate(folder, includeList = true)
    assertEquals(Files.readString(routes), updated)
  }

  test("regeneration updates request body types inside implemented for-comprehensions") {
    val folder = Files.createTempDirectory("openapi4s-incremental-body")
    generate(folder)
    val routes = folder.resolve("example/routes/UsersRoutes.scala")
    Files.writeString(
      routes,
      Files
        .readString(routes)
        .replace(
          "reqBody <- req.as[User]; resp <- NotImplemented(\"TODO: return User\")",
          "reqBody <- req.as[User]; resp <- Created(reqBody)"
        )
    )

    generate(folder, requestType = "NewUser")
    val updated = Files.readString(routes)
    assert(updated.contains("reqBody <- req.as[NewUser]"))
    assert(!updated.contains("req.as[User]"))
    assert(updated.contains("Created(reqBody)"))
    assertEquals(occurrences(updated, "resp <-"), 2)
  }

  test("regeneration updates Iron constraints while preserving newtype members") {
    val folder = Files.createTempDirectory("openapi4s-incremental-iron")
    generate(folder)
    val newtypes = folder.resolve("example/models/Newtypes.scala")
    val source = Files.readString(newtypes)
    val age = "object Age extends io.github.iltotore.iron.RefinedType[Int, GreaterEqual[0]]"
    assert(source.contains(age))
    Files.writeString(newtypes, source.replace(age, age + " { def description = \"user age\" }"))

    generate(folder, minimumAge = 18)
    val updated = Files.readString(newtypes)
    assert(updated.contains("GreaterEqual[18]"))
    assert(!updated.contains("GreaterEqual[0]"))
    assert(updated.contains("def description = \"user age\""))
    generate(folder, minimumAge = 18)
    assertEquals(Files.readString(newtypes), updated)
  }

  test("regenerated http4s and Iron sources compile and retain runtime behavior") {
    val folder = Files.createTempDirectory("openapi4s-incremental-runtime")
    generate(folder)
    val routes = folder.resolve("example/routes/UsersRoutes.scala")
    Files.writeString(
      routes,
      Files
        .readString(routes)
        .replace(
          "reqBody <- req.as[User]; resp <- NotImplemented(\"TODO: return User\")",
          "reqBody <- req.as[User]; resp <- Created(reqBody)"
        )
    )
    generate(folder, includeList = true, requestType = "NewUser", minimumAge = 18)
    val check = getClass.getResourceAsStream("/http4s_incremental_check.scala")
    try Files.copy(check, folder.resolve("IncrementalCheck.scala"), StandardCopyOption.REPLACE_EXISTING)
    finally check.close()
    CompilationTestUtils.runGenerated(
      folder,
      scalaVersion = "3.9.0",
      dependencies = Seq(
        "org.http4s::http4s-dsl:0.23.38",
        "org.http4s::http4s-circe:0.23.38",
        "org.http4s::http4s-server:0.23.38",
        "io.circe::circe-generic:0.14.17",
        "io.github.iltotore::iron:3.4.1",
        "io.github.iltotore::iron-circe:3.4.1"
      ),
      mainClass = "IncrementalCheck"
    )
  }

  private def occurrences(source: String, text: String): Int =
    source.sliding(text.length).count(_ == text)

  private def generate(
      folder: Path,
      includeList: Boolean = false,
      requestType: String = "User",
      minimumAge: Int = 0
  ): Unit = {
    val listOperation = if (includeList) """
    get:
      tags: [Users]
      responses:
        '200':
          description: Users
          content:
            application/json:
              schema: { type: array, items: { $ref: '#/components/schemas/User' } }
""" else ""
    val spec = s"""openapi: 3.0.3
info: { title: Incremental users, version: 1.0.0 }
paths:
  /users:
$listOperation
    post:
      tags: [Users]
      requestBody:
        required: true
        content:
          application/json:
            schema: { $$ref: '#/components/schemas/$requestType' }
      responses:
        '201':
          description: Created
          content:
            application/json:
              schema: { $$ref: '#/components/schemas/User' }
  /users/{id}:
    get:
      tags: [Users]
      parameters:
        - { name: id, in: path, required: true, schema: { type: string } }
      responses:
        '200':
          description: User
          content:
            application/json:
              schema: { $$ref: '#/components/schemas/User' }
components:
  schemas:
    User:
      type: object
      required: [age]
      properties:
        age: { type: integer, format: int32, minimum: $minimumAge }
    NewUser:
      type: object
      required: [age]
      properties:
        age: { type: integer, format: int32, minimum: $minimumAge }
"""
    val specFile = folder.resolve("openapi.yaml")
    Files.writeString(specFile, spec)
    OpenApiWriter(
      OpenApiWriter.Config(specFile.toString, folder, "example", "circe", "http4s", validation = "iron")
    ).write()
    ()
  }
}
