name := "FunctionalConcurrencyPrimitivesExamples"

version := "0.1"

scalaVersion := "2.12.11"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "2.0.0",
  "io.github.timwspence" %% "cats-stm" % "0.7.0",
  "org.typelevel" %% "kittens" % "2.0.0",
  "org.http4s" %% "http4s-dsl" % "0.20.16",
  "org.http4s" %% "http4s-blaze-client" % "0.20.16"
)
