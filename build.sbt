import BuildHelper._
import Dependencies._

globalSettings

lazy val root = project
  .in(file("."))
  .settings(
    welcomeMessage,
    skip in publish := true
  )
  .aggregate(core)

val core = project.module
  .settings(
    name := "validare-core",
    version := "0.5",
    libraryDependencies ++= Seq(
      catsCore,
      catsKernel,
      scalaTest,
      silencerLib
    )
  )
  .settings(Publish.publishSettings)
