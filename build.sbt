name := "Lambda-Calculator"

version := "1.0"

scalaVersion := "2.11.0"

lazy val core = project in file(".") dependsOn(scalaParserCombinators) settings(
  libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"
)

lazy val scalaParserCombinators = project in file("modules/scala-parser-combinators")

lazy val js = project in file("js") dependsOn(core) settings(
  scalaVersion <<= scalaVersion in core,
  unmanagedSourceDirectories in Compile <+= (baseDirectory in core) {
    _ / "src" / "main" / "scala"
  }
)
