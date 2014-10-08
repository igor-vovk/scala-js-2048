scalaJSSettings

name := "2048"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.2"

ScalaJSKeys.persistLauncher := true

libraryDependencies += "org.scala-lang.modules.scalajs" %%% "scalajs-dom" % "0.6"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.11.6" % "test"
)