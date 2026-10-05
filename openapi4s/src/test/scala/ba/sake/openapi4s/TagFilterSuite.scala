package ba.sake.openapi4s

class TagFilterSuite extends munit.FunSuite {

  private val definition = OpenApiDefinition.parse(TestUtils.getResourceUrl("sttp_client.yaml"))
  private def tagsOf(d: OpenApiDefinition) = d.pathDefinitions.defs.map(_.getTag).distinct.sorted

  test("no filters keeps everything") {
    assertEquals(tagsOf(OpenApiWriter.filterByTags(definition, None, None)), tagsOf(definition))
  }

  test("include is case-insensitive") {
    assertEquals(tagsOf(OpenApiWriter.filterByTags(definition, Some(List("PET")), None)), List("pet"))
  }

  test("exclude only") {
    assert(!tagsOf(OpenApiWriter.filterByTags(definition, None, Some(List("pet")))).contains("pet"))
  }

  test("exclude wins over include") {
    val res = OpenApiWriter.filterByTags(definition, Some(List("pet", "store")), Some(List("pet")))
    assertEquals(tagsOf(res), List("store"))
  }

  test("unknown tags are ignored") {
    assertEquals(OpenApiWriter.filterByTags(definition, Some(List("nope")), None).pathDefinitions.defs, Nil)
    assertEquals(tagsOf(OpenApiWriter.filterByTags(definition, None, Some(List("nope")))), tagsOf(definition))
  }
}
