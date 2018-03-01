
scalaVersion := "2.12.3"

name := "mics-hashcode"
organization := "com.mediarithmics"
version := "1.0"


libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.0.1",
  "org.typelevel" %% "cats-effect" % "0.9",
  "io.monix" %% "monix" % "2.3.3",

  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  "com.typesafe" % "config" % "1.3.1"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.3",
  "org.mockito" % "mockito-all" % "1.10.8",
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.8"
) map (_ % Test)

