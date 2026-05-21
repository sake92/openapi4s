package ba.sake.openapi4s

import ba.sake.regenesca.GeneratedFileSource

/** Base trait for OpenApi generators. Each generator (model or framework) implements this trait to produce generated
  * sources.
  */
trait OpenApiGenerator {
  def generate(): Seq[GeneratedFileSource]
}
