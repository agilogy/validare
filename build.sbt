import BuildHelper._
import Dependencies._

globalSettings

lazy val root = project
  .in(file("."))
  .settings(
    welcomeMessage,
    skip in publish := true
  )

val core = project.module
  .settings(
    name := "validare-core",
    version := "0.2",
    libraryDependencies ++= Seq(
      catsCore,
      scalaTest
    )
  )
  .settings(Publish.publishSettings: _*)
