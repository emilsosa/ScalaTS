ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "scala-ts"
  )

libraryDependencies += "org.reflections" % "reflections" % "0.10.2"
libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.10"
libraryDependencies += "org.slf4j" % "slf4j-simple" % "2.0.7"
