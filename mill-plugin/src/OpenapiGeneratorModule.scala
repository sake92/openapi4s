package ba.sake.openapi4s

import mill._
import mill.scalalib._

trait OpenApiGeneratorModule extends JavaModule {

  def openApi4sPackage: T[String]

  def openApi4sFile: T[PathRef] = Task(PathRef(millSourcePath / "resources" / "openapi.json"))

  def openApi4sTargetDir: T[PathRef] = Task(PathRef(millSourcePath / "src"))

  def openApi4sGenerator: T[String] = "sharaf"

  def openApi4sGenerate(): Command[Unit] = Task.Command {
    println("Starting to generate OpenApi sources...")
    val config = OpenApiGenerator.Config(
      url = openApi4sFile().path.wrapped.toUri.toString,
      baseFolder = openApi4sTargetDir().path.wrapped,
      basePackage = openApi4sPackage()
    )
    val generator = OpenApiGenerator(openApi4sGenerator(), config)
    generator.generate()
    println("Finished generating OpenApi sources")
  }

}
