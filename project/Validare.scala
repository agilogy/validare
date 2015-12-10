import sbt._
import sbt.Keys._

object Validare extends BaseBuild {

  lazy val validare = project.in(file("."))
    .settings(moduleName := "root")
    .settings(commonSettings)
    .settings(noPublishSettings)
    .aggregate(core, inContext, standalone)


  lazy val core = project.in(file("core"))
    .settings(moduleName := "validare-core")
    .settings(version := "0.1-SNAPSHOT")
    .settings(commonSettings)
    .settings(
      libraryDependencies ++= Seq(
        "com.agilogy" %% "classis-monoid" % "0.1-SNAPSHOT"
      )
    )
  //    .settings(
  //      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _),
  //      libraryDependencies ++= Seq("com.agilogy" %% "classis-monoid" % "0.1-SNAPSHOT")
  //    )

  lazy val inContext = project.in(file("inContext"))
    .settings(moduleName := "validare-in-context")
    .settings(version := "0.1-SNAPSHOT")
    .dependsOn(core)
    .settings(commonSettings)
    .settings(
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "2.2.4" % "test"
      )
    )

  lazy val standalone = project.in(file("standalone"))
    .settings(moduleName := "validare-standalone")
    .settings(version := "0.1-SNAPSHOT")
    .dependsOn(inContext)
    .settings(commonSettings)
    .settings(
      libraryDependencies ++= Seq(
        "com.agilogy" %% "classis-applicative" % "0.1-SNAPSHOT",
        "org.scalatest" %% "scalatest" % "2.2.4" % "test"
      )
    )

}
