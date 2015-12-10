//scalaVersion := "2.11.7"
//
//name := "Validare"
//
//organization := "agilogy"
//
//version := "0.1-SNAPSHOT"
//
//// --> Linters
//
//// See tinyurl.com/sd15lint
//
//// X checkinit
//// https://tpolecat.github.io/2014/04/11/scalac-flags.html
//scalacOptions ++= Seq(
//  "-deprecation",
//  "-encoding", "UTF-8",       // yes, this is 2 args
//  "-feature",
//  "-language:existentials",
//  "-language:higherKinds",
//  "-language:implicitConversions",
//  "-unchecked",
//  "-Xlint",
//  "-Yno-adapted-args",
//  "-Ywarn-numeric-widen",
//  "-Ywarn-value-discard",
//  "-Xfuture",
//  "-Xfatal-warnings",
//  "-Ywarn-unused-import",
//  "-Ywarn-dead-code",
//  "-P:linter:disable:PreferIfToBooleanMatch"
//)
//
//wartremoverErrors ++= Warts.allBut(Wart.DefaultArguments, Wart.MutableDataStructures)
//
//// Execute static analysis via `lint:compile`
////val LintTarget = config("lint").extend(Compile)
////
////inConfig(LintTarget) {
////
////  Defaults.compileSettings ++
////    Seq(
////      sources in LintTarget := {
////        val lintSources = (sources in LintTarget).value
////        lintSources ++ (sources in Compile).value
////      },
////      scalacOptions in LintTarget ++= Seq(
////        "-Xfatal-warnings",
////        "-Ywarn-unused-import",
////        "-Ywarn-dead-code",
////        "-P:linter:disable:PreferIfToBooleanMatch"
////      ),
////      wartremoverErrors ++= Warts.allBut(Wart.DefaultArguments, Wart.MutableDataStructures)
////    )
////}
//
//scalacOptions in Compile := (scalacOptions in Compile).value filterNot { switch =>
//  switch.startsWith("-P:wartremover:") ||
//    "^-Xplugin:.*/org[.]brianmckenna/.*wartremover.*[.]jar$".r.pattern.matcher(switch).find ||
//    switch.startsWith("-P:linter:") ||
//    "^-Xplugin:.*/com[.]foursquare[.]lint/.*linter.*[.]jar$".r.pattern.matcher(switch).find
//}
//
//resolvers += "Linter Repository" at "https://hairyfotr.github.io/linteRepo/releases"
//
//addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.12")
//
//scalastyleFailOnError := true
//
//// <-- Linters
//
//// Reformat at every compile.
//// See https://github.com/sbt/sbt-scalariform
//scalariformSettings
//
//ScoverageSbtPlugin.ScoverageKeys.coverageExcludedPackages := "<empty>"
//
//publishMavenStyle := false
