name := "Lambda-Calculator"

version := "1.0"

scalaVersion := "2.11.0"

resolvers += Resolver.url(
  "bintray-scala-js-releases",
    url("http://dl.bintray.com/content/scala-js/scala-js-releases"))(
        Resolver.ivyStylePatterns)

lazy val core = project in file(".") settings(
  libraryDependencies ++= Seq(
    "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test",
    "org.scalajs" %%% "scala-parser-combinators" % "1.0.1"
  )
)

lazy val js = project in file("js") dependsOn(core) settings(
  scalaVersion <<= scalaVersion in core,
  unmanagedSourceDirectories in Compile <+= (baseDirectory in core) {
    _ / "src" / "main" / "scala"
  }
)
