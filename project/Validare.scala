import sbt._
import sbt.Keys._

object Validare extends BaseBuild {

  lazy val validare = project.in(file("."))
    .settings(moduleName := "root")
    .settings(commonSettings)
    .settings(noPublishSettings)
    .aggregate(path, core, parsing)


  lazy val path = project.in(file("path"))
    .settings(moduleName := "path")
    .settings(version := "0.1-SNAPSHOT")
    .settings(commonSettings)
    .settings(
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "2.2.4" % "test"
      )
    )

  lazy val core = project.in(file("core"))
    .settings(moduleName := "validare-core")
    .settings(version := "0.1-SNAPSHOT")
    .settings(commonSettings)
    .settings(
      libraryDependencies ++= Seq(
        "com.agilogy" %% "either-extras" % "0.1.1-20160810",
        "org.scalatest" %% "scalatest" % "2.2.4" % "test"
      )
    )

  lazy val parsing = project.in(file("parsing"))
    .settings(moduleName := "validare-parsing")
    .settings(version := "0.1-SNAPSHOT")
    .settings(commonSettings)
    .settings(
      libraryDependencies ++= Seq(
        "com.agilogy" %% "either-extras" % "0.1.1-20160810",
        "org.scalatest" %% "scalatest" % "2.2.4" % "test"
      )
    )
    .dependsOn(core)
  //    .settings(
  //      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _),
  //      libraryDependencies ++= Seq("com.agilogy" %% "classis-monoid" % "0.1-SNAPSHOT")
  //    )

}
