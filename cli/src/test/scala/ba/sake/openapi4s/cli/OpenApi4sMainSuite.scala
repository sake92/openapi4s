package ba.sake.openapi4s.cli

class OpenApi4sMainSuite extends munit.FunSuite {

  test("no framework/client flags -> models only") {
    assertEquals(OpenApi4sMain.resolveBackends("tupson", None, None), ("tupson", "none", "none"))
  }

  test("--framework only -> server only") {
    assertEquals(OpenApi4sMain.resolveBackends("tupson", Some("http4s"), None), ("tupson", "http4s", "none"))
    assertEquals(OpenApi4sMain.resolveBackends("tupson", Some("sharaf"), None), ("tupson", "sharaf", "none"))
  }

  test("--client only -> client only (no server)") {
    assertEquals(OpenApi4sMain.resolveBackends("tupson", None, Some("sttp")), ("tupson", "none", "sttp"))
  }

  test("--framework and --client -> both") {
    assertEquals(OpenApi4sMain.resolveBackends("circe", Some("sharaf"), Some("sttp")), ("circe", "sharaf", "sttp"))
  }

  test("values are case-insensitive") {
    assertEquals(OpenApi4sMain.resolveBackends("Tupson", Some("Sharaf"), Some("STTP")), ("tupson", "sharaf", "sttp"))
  }

  test("--models none is rejected (models are mandatory)") {
    interceptMessage[RuntimeException](
      "Invalid --models value 'none'. Models are mandatory; use 'circe' or 'tupson'."
    ) {
      OpenApi4sMain.resolveBackends("none", None, None)
    }
  }

  test("--framework none is rejected") {
    interceptMessage[RuntimeException](
      "Invalid --framework value 'none'. To generate no server, omit --framework entirely. Available server frameworks: 'http4s', 'sharaf'."
    ) {
      OpenApi4sMain.resolveBackends("tupson", Some("none"), None)
    }
  }

  test("--client none is rejected") {
    interceptMessage[RuntimeException](
      "Invalid --client value 'none'. To generate no client, omit --client entirely. Available client backends: 'sttp'."
    ) {
      OpenApi4sMain.resolveBackends("tupson", None, Some("none"))
    }
  }

  test("unknown model backend is rejected") {
    interceptMessage[RuntimeException]("Unknown model backend 'bogus'. Available model backends: 'circe', 'tupson'.") {
      OpenApi4sMain.resolveBackends("bogus", None, None)
    }
  }

  test("unknown framework backend is rejected") {
    interceptMessage[RuntimeException](
      "Unknown framework backend 'bogus'. Available framework backends: 'http4s', 'sharaf'."
    ) {
      OpenApi4sMain.resolveBackends("tupson", Some("bogus"), None)
    }
  }

  test("unknown client backend is rejected") {
    interceptMessage[RuntimeException]("Unknown client backend 'bogus'. Available client backends: 'sttp'.") {
      OpenApi4sMain.resolveBackends("tupson", None, Some("bogus"))
    }
  }
}
