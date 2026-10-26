package ba.sake.openapi4s

import com.typesafe.scalalogging.LazyLogging

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.time.Duration
import java.util.Locale
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Try
import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ObjectNode
import io.swagger.v3.core.util.Json
import io.swagger.v3.oas.models.media.Schema
import io.swagger.v3.parser.util.OpenAPIDeserializer

/** Adapts JSON Schema documents to the generator's model-only input. */
object JsonSchemaDefinition extends LazyLogging {

  private sealed trait Input
  private object Input {
    case class RemoteDocument(uri: URI, root: JsonNode) extends Input
    case class LocalPath(path: Path) extends Input
  }

  private case class Document(uri: URI, rootName: String, root: JsonNode, definitions: Map[String, JsonNode])

  private lazy val httpClient = HttpClient
    .newBuilder()
    .followRedirects(HttpClient.Redirect.NORMAL)
    .connectTimeout(Duration.ofSeconds(10))
    .build()

  def isInput(input: String): Boolean =
    try detect(input).isDefined
    catch {
      case e: RuntimeException =>
        logger.debug(s"Could not detect JSON Schema input '$input': ${e.getMessage}")
        false
    }

  def parse(input: String): OpenApiDefinition =
    tryParse(input).getOrElse {
      throw new RuntimeException(
        s"JSON Schema input '$input' must be a JSON Schema file, directory, or HTTP(S) URL."
      )
    }

  private[openapi4s] def tryParse(input: String): Option[OpenApiDefinition] =
    detect(input).map {
      case Input.RemoteDocument(uri, root) =>
        parseDocuments(loadDocuments(List(uri), Map(uri -> root)))
      case Input.LocalPath(path) =>
        val uris = schemaFiles(path).map(fileUri)
        if (uris.isEmpty) {
          throw new RuntimeException(s"JSON Schema input '$input' contains no .json schema files.")
        }
        parseDocuments(loadDocuments(uris))
    }

  private def parseDocuments(documents: List[Document]): OpenApiDefinition = {
    ensureUniqueNames(documents)

    val rootNames = documents.map(document => document.uri -> document.rootName).toMap
    val definitionNames = assignDefinitionNames(documents, rootNames.values.toList)
    ensureUniqueComponentNames(rootNames.values.toList ++ definitionNames.values.toList)

    val components: Map[String, Schema[?]] = documents.flatMap { document =>
      val root = parseSchema(rewriteRefs(document.root, document, rootNames, definitionNames), document.rootName)
      val definitions = document.definitions.toList.map { case (key, definition) =>
        val name = definitionNames((document.uri, s"/$$defs/${escapeJsonPointer(key)}"))
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

  private def detect(input: String): Option[Input] =
    inputUri(input).flatMap { uri =>
      scheme(uri) match {
        case Some("http" | "https") => detectRemote(uri)
        case _                      => detectLocal(Paths.get(uri))
      }
    }

  private def detectRemote(uri: URI): Option[Input] = {
    val isYaml = Option(uri.getPath).exists { path =>
      val lower = path.toLowerCase(Locale.ROOT)
      lower.endsWith(".yaml") || lower.endsWith(".yml")
    }
    if (isYaml) None
    else {
      val root = loadRemoteRoot(uri)
      root.filter(isJsonSchemaDocument).map(json => Input.RemoteDocument(uri, json))
    }
  }

  private def loadRemoteRoot(uri: URI): Option[JsonNode] =
    try Some(fetchJson(uri))
    catch {
      case e: RuntimeException if !hasJsonExtension(uri) =>
        logger.debug(s"Could not read '$uri' as JSON; leaving it to the OpenAPI parser: ${e.getMessage}")
        None
      case e: RuntimeException => throw e
    }

  private def detectLocal(path: Path): Option[Input] =
    if (Files.isDirectory(path)) Some(Input.LocalPath(path))
    else if (Files.isRegularFile(path) && isJsonDocumentWithoutOpenApiMarker(path)) Some(Input.LocalPath(path))
    else None

  private def inputUri(input: String): Option[URI] = {
    val absolute = Try(new URI(input)).toOption.filter(_.isAbsolute)
    absolute match {
      case Some(uri) =>
        scheme(uri) match {
          case Some("file" | "http" | "https") => Some(normalizeUri(uri))
          case _                               => localUri(input)
        }
      case None => localUri(input)
    }
  }

  private def localUri(input: String): Option[URI] =
    Try(Paths.get(input).toAbsolutePath.normalize.toUri).toOption

  private def fileUri(path: Path): URI = normalizeUri(path.toAbsolutePath.normalize.toUri)

  private def scheme(uri: URI): Option[String] =
    Option(uri.getScheme).map(_.toLowerCase(Locale.ROOT))

  private def normalizeUri(uri: URI): URI = {
    val normalizedScheme = Option(uri.getScheme).map(_.toLowerCase(Locale.ROOT)).orNull
    val withoutFragment = new URI(normalizedScheme, uri.getAuthority, uri.getPath, uri.getQuery, null)
    withoutFragment.normalize
  }

  private def fetchJson(uri: URI): JsonNode = {
    val request = HttpRequest
      .newBuilder(uri)
      .timeout(Duration.ofSeconds(30))
      .header("Accept", "application/json, application/schema+json, */*")
      .header("User-Agent", "openapi4s")
      .GET()
      .build()
    val response =
      try httpClient.send(request, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))
      catch {
        case e: InterruptedException =>
          Thread.currentThread().interrupt()
          throw new RuntimeException(s"Could not fetch document from '$uri': ${e.getMessage}", e)
        case e: Exception =>
          throw new RuntimeException(s"Could not fetch document from '$uri': ${e.getMessage}", e)
      }
    val status = response.statusCode()
    if (status < 200 || status >= 300) {
      throw new RuntimeException(s"Could not fetch document from '$uri': HTTP $status.")
    }
    parseJson(response.body(), uri)
  }

  private def parseJson(content: String, source: URI): JsonNode =
    try Json.mapper().readTree(content)
    catch {
      case e: Exception => throw new RuntimeException(s"Could not parse JSON from '$source': ${e.getMessage}", e)
    }

  private def loadDocuments(initialUris: List[URI], preloaded: Map[URI, JsonNode] = Map.empty): List[Document] = {
    val documents = mutable.LinkedHashMap.empty[URI, Document]
    val pending = mutable.Queue.from(initialUris.distinct)
    while (pending.nonEmpty) {
      val uri = pending.dequeue()
      if (!documents.contains(uri)) {
        val document = loadDocument(uri, preloaded.get(uri))
        documents += uri -> document
        referencedUris(document).foreach { target =>
          if (!documents.contains(target)) pending.enqueue(target)
        }
      }
    }
    documents.values.toList
  }

  private def loadDocument(uri: URI, preloadedRoot: Option[JsonNode]): Document = {
    val root = preloadedRoot.getOrElse(readJson(uri))
    if (root == null || !root.isObject) {
      throw new RuntimeException(s"JSON Schema document '$uri' must contain an object schema.")
    }
    val name =
      Option(root.get("title")).filter(_.isTextual).map(_.asText.trim).filter(_.nonEmpty).getOrElse(fileStem(uri))
    val definitions = Option(root.get("$defs")) match {
      case None                         => Map.empty[String, JsonNode]
      case Some(defs) if !defs.isObject =>
        throw new RuntimeException(s"JSON Schema '$uri' has a non-object '$$defs'.")
      case Some(defs) => defs.fields.asScala.map(entry => entry.getKey -> entry.getValue).toMap
    }
    Document(uri, name, root, definitions)
  }

  private def readJson(uri: URI): JsonNode =
    scheme(uri) match {
      case Some("http" | "https") => fetchJson(uri)
      case Some("file") | None    => readFile(uri)
      case Some(other)            =>
        throw new RuntimeException(s"Unsupported JSON Schema URI scheme '$other' for '$uri'.")
    }

  private def readFile(uri: URI): JsonNode = {
    val path = Paths.get(uri)
    try Json.mapper().readTree(path.toFile)
    catch {
      case e: Exception => throw new RuntimeException(s"Could not parse JSON Schema file '$path': ${e.getMessage}", e)
    }
  }

  private def referencedUris(document: Document): List[URI] = {
    val targets = mutable.ListBuffer.empty[URI]
    def visit(current: JsonNode): Unit = {
      if (current.isObject) {
        Option(current.get("$ref")).filter(_.isTextual).foreach { ref =>
          externalRefUri(ref.asText, document.uri).foreach { target =>
            ensureAllowedReference(document.uri, target)
            targets += target
          }
        }
        current.properties.asScala.foreach(entry => visit(entry.getValue))
      } else if (current.isArray) current.elements.asScala.foreach(visit)
    }
    visit(document.root)
    targets.toList
  }

  private def externalRefUri(ref: String, source: URI): Option[URI] = {
    val hashIndex = ref.indexOf('#')
    val filePart = if (hashIndex < 0) ref else ref.substring(0, hashIndex)
    if (filePart.isEmpty) None else Some(resolveRefUri(source, filePart))
  }

  private def resolveRefUri(source: URI, filePart: String): URI =
    try normalizeUri(source.resolve(filePart))
    catch {
      case e: Exception =>
        throw new RuntimeException(
          s"Could not resolve JSON Schema reference '$filePart' from '$source': ${e.getMessage}",
          e
        )
    }

  private def ensureAllowedReference(source: URI, target: URI): Unit = {
    val sourceScheme = scheme(source)
    val targetScheme = scheme(target)
    val isRemoteSource = sourceScheme.contains("http") || sourceScheme.contains("https")
    val isRemoteTarget = targetScheme.contains("http") || targetScheme.contains("https")
    if (isRemoteSource && !isRemoteTarget) {
      throw new RuntimeException(
        s"Remote JSON Schema '$source' may only reference HTTP(S) URLs, but referenced '$target'."
      )
    }
    if (!isRemoteTarget && !targetScheme.contains("file")) {
      throw new RuntimeException(s"Unsupported JSON Schema reference URI '$target'.")
    }
  }

  private def isJsonDocumentWithoutOpenApiMarker(path: Path): Boolean =
    try {
      val root = Json.mapper().readTree(path.toFile)
      isJsonSchemaDocument(root)
    } catch {
      case _: Exception => path.getFileName.toString.toLowerCase(Locale.ROOT).endsWith(".json")
    }

  private def isJsonSchemaDocument(root: JsonNode): Boolean =
    root != null && root.isObject && !root.has("openapi") && !root.has("swagger")

  private def hasJsonExtension(uri: URI): Boolean =
    Option(uri.getPath).exists(_.toLowerCase(Locale.ROOT).endsWith(".json"))

  private def fileStem(uri: URI): String = {
    val path = Option(uri.getPath).getOrElse("")
    val fileName = path.substring(path.lastIndexOf('/') + 1)
    val lower = fileName.toLowerCase(Locale.ROOT)
    val stem =
      if (lower.endsWith(".json")) fileName.dropRight(".json".length)
      else {
        val dot = fileName.lastIndexOf('.')
        if (dot > 0) fileName.substring(0, dot) else fileName
      }
    if (stem.nonEmpty) stem else "Schema"
  }

  private def schemaFiles(input: Path): List[Path] = {
    if (Files.isRegularFile(input)) List(input)
    else if (Files.isDirectory(input)) {
      val stream = Files.walk(input)
      try {
        stream
          .iterator()
          .asScala
          .filter(path =>
            Files.isRegularFile(path) && path.getFileName.toString.toLowerCase(Locale.ROOT).endsWith(".json")
          )
          .map(_.toAbsolutePath.normalize)
          .toList
          .sortBy(_.toString)
      } finally stream.close()
    } else throw new RuntimeException(s"JSON Schema input '$input' does not exist or is not a regular file/directory.")
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
    val duplicates = names.groupBy(_.capitalize).collect { case (name, occurrences) if occurrences.size > 1 => name }
    if (duplicates.nonEmpty) {
      throw new RuntimeException(
        s"JSON Schema component names must be unique; duplicates: ${duplicates.toList.sorted.mkString(", ")}."
      )
    }
  }

  private def assignDefinitionNames(documents: List[Document], rootNames: List[String]): Map[(URI, String), String] = {
    val entries = documents.flatMap { document =>
      document.definitions.keys.map { key =>
        val pointer = document.uri -> s"/$$defs/${escapeJsonPointer(key)}"
        (pointer, key.capitalize, s"${document.rootName}__${key}".capitalize)
      }
    }
    val fallbackNames = entries.map { case (pointer, _, fallback) => pointer -> fallback }.toMap
    val initialNames = entries.map { case (pointer, name, _) => pointer -> name }.toMap

    // Compare the names emitted by both backends, including capitalization. A fallback
    // can itself collide with a short name, so qualify those definitions on the next pass.
    @scala.annotation.tailrec
    def qualifyCollisions(names: Map[(URI, String), String]): Map[(URI, String), String] = {
      val duplicates = (rootNames ++ names.values)
        .groupBy(_.capitalize)
        .collect {
          case (name, occurrences) if occurrences.size > 1 => name
        }
        .toSet
      val qualified = names.map { case (pointer, name) =>
        pointer -> (if (duplicates(name.capitalize)) fallbackNames(pointer) else name)
      }
      if (qualified == names) names else qualifyCollisions(qualified)
    }

    qualifyCollisions(initialNames)
  }

  private def rewriteRefs(
      original: JsonNode,
      source: Document,
      rootNames: Map[URI, String],
      definitionNames: Map[(URI, String), String]
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
      rootNames: Map[URI, String],
      definitionNames: Map[(URI, String), String]
  ): String = {
    val hashIndex = ref.indexOf('#')
    val (filePart, fragment) =
      if (hashIndex < 0) (ref, "")
      else (ref.substring(0, hashIndex), ref.substring(hashIndex + 1))
    val targetUri = if (filePart.isEmpty) source.uri else resolveRefUri(source.uri, filePart)
    val pointer = if (fragment.isEmpty) "" else fragment
    val targetName = pointer match {
      case "" => rootNames.get(targetUri)
      case defsPointer if defsPointer.startsWith("/$defs/") && !defsPointer.drop("/$defs/".length).contains("/") =>
        definitionNames.get(targetUri -> defsPointer)
      case _ => None
    }
    targetName
      .map(name => s"#/components/schemas/$name")
      .getOrElse {
        throw new RuntimeException(
          s"Unsupported or unresolved JSON Schema reference '$ref' in '${source.uri}'. " +
            "Supported references target a schema document, its root (#), or one of its top-level $defs entries."
        )
      }
  }

  private def parseSchema(node: JsonNode, name: String): Schema[?] = {
    val result = new OpenAPIDeserializer.ParseResult().openapi31(true)
    val schema = new OpenAPIDeserializer().getJsonSchema(node, name, result)
    if (schema == null) {
      throw new RuntimeException(s"Could not parse JSON Schema '$name': ${result.getMessages.asScala.mkString("; ")}")
    }
    result.getMessages.asScala.foreach(message => logger.warn(s"JSON Schema '$name' had parser issue: $message"))
    schema
  }

  private def escapeJsonPointer(value: String): String = value.replace("~", "~0").replace("/", "~1")
}
