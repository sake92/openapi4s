package ba.sake.openapi4s

import mill._
import mill.scalalib._

trait OpenApiGeneratorModule extends JavaModule {

  def openApi4sUrl: T[String] = T((millSourcePath / "resources" / "openapi.json").wrapped.toUri.toString)
  def openApi4sTargetDir: T[os.Path] = T(millSourcePath / "src")

  def openApi4sGenerator: T[String] = "sharaf"
  def openApi4sPackage: T[String]

  def openApi4sGenerate(): Command[Unit] = T.command {
    println("Starting to generate OpenApi sources...")
    val config = OpenApiGenerator.Config(
      url = openApi4sUrl(),
      baseFolder = openApi4sTargetDir().wrapped,
      basePackage = openApi4sPackage()
    )
    val generator = OpenApiGenerator(openApi4sGenerator(), config)
    generator.generate()
    println("Finished generating OpenApi sources")
  }

}
