scalaJSSettings

name := "Lambda-Calculator"

version := "1.0"

scalaVersion := "2.11.0"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"

// lazy val root = project in file(".") dependsOn(scalaParserCombinators) aggregate(scalaParserCombinators)

// lazy val scalaParserCombinators = project in file("modules/scala-parser-combinators")

// libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"

libraryDependencies += "org.scalajs" %%% "scala-parser-combinators" % "1.0.2-SNAPSHOT"
