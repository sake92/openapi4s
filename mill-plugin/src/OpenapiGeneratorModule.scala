package ba.sake.openapi4s

import mill._
import mill.scalalib._

trait OpenapiGeneratorModule extends JavaModule {

  def openapi4sUrl: T[String] = T((millSourcePath / "resources" / "openapi.json").wrapped.toUri.toString)
  def openapi4sTargetDir: T[os.Path] = T(millSourcePath / "src")

  def openapi4sGenerator: T[String] = "sharaf"
  def openapi4sPackage: T[String]

  def openapi4sGenerate(): Command[Unit] = T.command {
    println("Starting to generate openapi sources...")
    val config = OpenApiGenerator.Config(
      url = openapi4sUrl(),
      baseFolder = openapi4sTargetDir().wrapped,
      basePackage = openapi4sPackage()
    )
    val generator = OpenApiGenerator(openapi4sGenerator(), config)
    generator.generate()
    println("Finished generating openapi sources")
  }

}
