package ba.sake.openapi4s

class SchemaNormalizerSuite extends munit.FunSuite {
  override def munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(10, "min")
  import SchemaDefinition._
  import EnumLiteral._

  private def refined(base: SchemaDefinition, value: EnumLiteral): AllOf =
    AllOf(List(Obj(List(SchemaProperty("kind", base))), Obj(List(SchemaProperty("kind", Opt(Const(value, None)))))))

  test("scalar literal refinements preserve requiredness in either member order") {
    val pairs = List(
      Str(None, Some(2), Some(8), Some("ead")) -> StrValue("ready"),
      Int32(None, Some(10), Some(0)) -> IntValue(5),
      Int64(None, None, None) -> LongValue(Long.MaxValue),
      Num32(None, Some(10f), Some(0f)) -> NumValue(1.5),
      Num64(None, Some(10d), Some(0d)) -> IntValue(2),
      Bool(None) -> BoolValue(true)
    )
    pairs.foreach { case (base, value) =>
      val schema = refined(base, value)
      for (members <- List(schema.schemas, schema.schemas.reverse)) {
        assertEquals(
          SchemaNormalizer.normalize(AllOf(members), Map.empty, "test"),
          Obj(List(SchemaProperty("kind", Const(value, None))))
        )
      }
    }
  }

  test("incompatible scalar literals and violated constraints fail") {
    val pairs = List(
      Str(None, None, None, None) -> IntValue(1),
      Str(None, Some(6), None, None) -> StrValue("ready"),
      Str(None, None, Some(4), None) -> StrValue("ready"),
      Str(None, None, None, Some("^done$")) -> StrValue("ready"),
      Int32(None, None, None) -> LongValue(Long.MaxValue),
      Int64(None, None, None) -> NumValue(1.5),
      Int32(None, Some(1), None) -> IntValue(2),
      Num64(None, None, Some(2d)) -> NumValue(1.5),
      Bool(None) -> StrValue("true")
    )
    pairs.foreach { case (base, value) =>
      intercept[exceptions.UnsupportedSchemaException] {
        SchemaNormalizer.normalize(refined(base, value), Map.empty, "test")
      }
    }
  }

  test("anyOf branches remain alternatives while nested allOf is normalized") {
    val literal = Const(StrValue("ready"), None)
    val other = Obj(List(SchemaProperty("other", Bool(None))))
    val schema = AnyOf(List(refined(Str(None, None, None, None), literal.value), other))
    assertEquals(
      SchemaNormalizer.normalize(schema, Map.empty, "test"),
      AnyOf(List(Obj(List(SchemaProperty("kind", literal))), other))
    )
  }

  for (backend <- List("circe", "tupson")) {
    test(s"$backend generates and compiles a refined object with inherited fields") {
      val base =
        Obj(List(SchemaProperty("kind", Str(None, None, None, None)), SchemaProperty("count", Int64(None, None, None))))
      val intersection =
        AllOf(List(Ref("Base"), Obj(List(SchemaProperty("kind", Opt(Const(StrValue("ready"), None)))))))
      val definition = OpenApiDefinition(
        NamedSchemaDefinitions(Seq(Named("Base", base), Named("Refined", intersection))),
        PathDefinitions(List.empty)
      )
      val config = OpenApiWriter.Config(
        url = "unused",
        baseFolder = java.nio.file.Files.createTempDirectory("shared-intersection"),
        basePackage = "example",
        models = backend,
        framework = "none"
      )
      val sources = ModelBackend.byId(ModelBackendId.fromString(backend)).generator(config, definition).generate()
      val generated = sources.last.toString
      assert(generated.contains("count: Long"), generated)
      assert(generated.contains(if (backend == "tupson") "kind: \"ready\"" else "kind: Json"), generated)
      // Use the writer for compilation so actual generated source files are checked.
      val schemaFile = config.baseFolder.resolve("schema.json")
      java.nio.file.Files.writeString(
        schemaFile,
        """{"title":"Refined","allOf":[{"type":"object","required":["kind","count"],"properties":{"kind":{"type":"string"},"count":{"type":"integer"}}},{"properties":{"kind":{"const":"ready"}}}]}"""
      )
      OpenApiWriter(config.copy(url = schemaFile.toString)).write()
      val rendered = java.nio.file.Files.readString(config.baseFolder.resolve("example/models/Refined.scala"))
      assert(rendered.contains("count: Long"), rendered)
      assert(rendered.contains(if (backend == "tupson") "kind: \"ready\"" else "kind: Json"), rendered)
      val dependencies =
        if (backend == "tupson") Seq("ba.sake::tupson:0.30.0", "ba.sake::validson:0.19.0")
        else Seq("io.circe::circe-core:0.14.10", "io.circe::circe-generic:0.14.10")
      CompilationTestUtils.compileGenerated(config.baseFolder.resolve("example"), "3.7.3", dependencies)
    }
  }
}
