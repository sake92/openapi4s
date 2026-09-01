package ba.sake.openapi4s

import java.net.URI
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._
import scala.util.Try
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ObjectNode
import io.swagger.v3.core.util.Json
import io.swagger.v3.oas.models.media.Schema
import io.swagger.v3.parser.util.OpenAPIDeserializer

/** Adapts local JSON Schema documents to the generator's model-only input. */
object JsonSchemaDefinition {

  private case class Document(path: Path, rootName: String, root: JsonNode, definitions: Map[String, JsonNode])

  def isInput(input: String): Boolean =
    localPath(input).exists { path =>
      Files.isDirectory(path) ||
      (Files.isRegularFile(path) && isJsonDocumentWithoutOpenApiMarker(path))
    }

  def parse(input: String): OpenApiDefinition = {
    val inputPath = localPath(input).getOrElse {
      throw new RuntimeException(s"JSON Schema input must be a local file or directory, got '$input'.")
    }
    val files = schemaFiles(inputPath)
    if (files.isEmpty) {
      throw new RuntimeException(s"JSON Schema input '$input' contains no .json schema files.")
    }

    val documents = files.map(loadDocument)
    ensureUniqueNames(documents)

    val rootNames = documents.map(document => document.path -> document.rootName).toMap
    val definitionNames: Map[(Path, String), String] = documents.flatMap { document =>
      document.definitions.keys.map { key =>
        (document.path, s"/$$defs/${escapeJsonPointer(key)}") -> s"${document.rootName}__${key}"
      }
    }.toMap
    ensureUniqueComponentNames(rootNames.values.toList ++ definitionNames.values.toList)

    val components: Map[String, Schema[?]] = documents.flatMap { document =>
      val root = parseSchema(rewriteRefs(document.root, document, rootNames, definitionNames), document.rootName)
      val definitions = document.definitions.toList.map { case (key, definition) =>
        val name = definitionNames((document.path, s"/$$defs/${escapeJsonPointer(key)}"))
        name -> parseSchema(rewriteRefs(definition, document, rootNames, definitionNames), name)
      }
      (document.rootName -> root) :: definitions
    }.toMap

    val resolver = new SchemaDefinitionResolver()
    OpenApiDefinition(
      namedSchemaDefinitions = resolver.resolveNamedSchemas(components),
      pathDefinitions = PathDefinitions(List.empty)
    ).normalized
  }

  private def localPath(input: String): Option[Path] = {
    val uriPath = Try {
      val uri = new URI(input)
      if (uri.getScheme == null) Paths.get(input)
      else if (uri.getScheme == "file") Paths.get(uri)
      else throw new IllegalArgumentException("not a local path")
    }.toOption
    uriPath.orElse(Try(Paths.get(input)).toOption).map(_.toAbsolutePath.normalize)
  }

  private def isJsonDocumentWithoutOpenApiMarker(path: Path): Boolean =
    try {
      val root = Json.mapper().readTree(path.toFile)
      !Option(root).exists(node => node.isObject && node.has("openapi"))
    } catch {
      case _: Exception => path.getFileName.toString.toLowerCase.endsWith(".json")
    }

  private def schemaFiles(input: Path): List[Path] = {
    if (Files.isRegularFile(input)) List(input)
    else if (Files.isDirectory(input)) {
      val stream = Files.walk(input)
      try {
        stream
          .iterator()
          .asScala
          .filter(path => Files.isRegularFile(path) && path.getFileName.toString.toLowerCase.endsWith(".json"))
          .map(_.toAbsolutePath.normalize)
          .toList
          .sortBy(_.toString)
      } finally stream.close()
    } else throw new RuntimeException(s"JSON Schema input '$input' does not exist or is not a regular file/directory.")
  }

  private def loadDocument(path: Path): Document = {
    val root =
      try Json.mapper().readTree(path.toFile)
      catch {
        case e: Exception => throw new RuntimeException(s"Could not parse JSON Schema file '$path': ${e.getMessage}", e)
      }
    if (root == null || !root.isObject) {
      throw new RuntimeException(s"JSON Schema file '$path' must contain an object schema.")
    }
    val name = Option(root.get("title")).filter(_.isTextual).map(_.asText.trim).filter(_.nonEmpty).getOrElse {
      val filename = path.getFileName.toString
      filename.substring(0, filename.length - ".json".length)
    }
    val definitions = Option(root.get("$defs")) match {
      case None => Map.empty[String, JsonNode]
      case Some(defs) if !defs.isObject =>
        throw new RuntimeException(s"JSON Schema '$path' has a non-object '$$defs'.")
      case Some(defs) => defs.fields.asScala.map(entry => entry.getKey -> entry.getValue).toMap
    }
    Document(path, name, root, definitions)
  }

  private def ensureUniqueNames(documents: List[Document]): Unit = {
    val duplicateNames = documents.groupBy(_.rootName).collect { case (name, docs) if docs.size > 1 => name }
    if (duplicateNames.nonEmpty) {
      throw new RuntimeException(
        s"JSON Schema model names must be unique; duplicates: ${duplicateNames.toList.sorted.mkString(", ")}."
      )
    }
  }

  private def ensureUniqueComponentNames(names: List[String]): Unit = {
    val duplicates = names.groupBy(identity).collect { case (name, occurrences) if occurrences.size > 1 => name }
    if (duplicates.nonEmpty) {
      throw new RuntimeException(
        s"JSON Schema component names must be unique; duplicates: ${duplicates.toList.sorted.mkString(", ")}."
      )
    }
  }

  private def rewriteRefs(
      original: JsonNode,
      source: Document,
      rootNames: Map[Path, String],
      definitionNames: Map[(Path, String), String]
  ): JsonNode = {
    val node = original.deepCopy[JsonNode]()
    def visit(current: JsonNode): Unit = {
      if (current.isObject) {
        val obj = current.asInstanceOf[ObjectNode]
        Option(obj.get("$ref")).filter(_.isTextual).foreach { ref =>
          obj.put("$ref", rewriteRef(ref.asText, source, rootNames, definitionNames))
        }
        obj.fields.asScala.foreach(entry => visit(entry.getValue))
      } else if (current.isArray) current.elements.asScala.foreach(visit)
    }
    visit(node)
    node
  }

  private def rewriteRef(
      ref: String,
      source: Document,
      rootNames: Map[Path, String],
      definitionNames: Map[(Path, String), String]
  ): String = {
    val hashIndex = ref.indexOf('#')
    val (filePart, fragment) =
      if (hashIndex < 0) (ref, "")
      else (ref.substring(0, hashIndex), ref.substring(hashIndex + 1))
    val targetPath =
      if (filePart.isEmpty) source.path
      else source.path.getParent.resolve(filePart).normalize.toAbsolutePath
    val pointer = if (fragment.isEmpty) "" else fragment
    val targetName = pointer match {
      case "" => rootNames.get(targetPath)
      case defsPointer if defsPointer.startsWith("/$defs/") && !defsPointer.drop("/$defs/".length).contains("/") =>
        definitionNames.get(targetPath -> defsPointer)
      case _ => None
    }
    targetName
      .map(name => s"#/components/schemas/$name")
      .getOrElse {
        throw new RuntimeException(
          s"Unsupported or unresolved JSON Schema reference '$ref' in '${source.path}'. " +
            "Supported references target a schema file, its root (#), or one of its top-level $defs entries."
        )
      }
  }

  private def parseSchema(node: JsonNode, name: String): Schema[?] = {
    val result = new OpenAPIDeserializer.ParseResult().openapi31(true)
    val schema = new OpenAPIDeserializer().getJsonSchema(node, name, result)
    if (schema == null) {
      throw new RuntimeException(s"Could not parse JSON Schema '$name': ${result.getMessages.asScala.mkString("; ")}")
    }
    result.getMessages.asScala.foreach(message => println(s"JSON Schema '$name' had parser issue: $message"))
    schema
  }

  private def escapeJsonPointer(value: String): String = value.replace("~", "~0").replace("/", "~1")
}
