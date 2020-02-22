import sbt._

trait Ghik {

  private val org             = "com.github.ghik"
  val ghik: String            = org
  private val silencerVersion = "1.4.4"

  val silencerLib         = org % "silencer-lib_2.12.10" % "1.4.4"
  val silencerLibProvided = org % "silencer-lib"         % silencerVersion % Provided cross CrossVersion.full
  val silencerCompilerPlugin = compilerPlugin(
    org % "silencer-plugin" % silencerVersion cross CrossVersion.full
  )
}

object Ghik extends Ghik

trait Typelevel {

  private val org         = "org.typelevel"
  private val catsVersion = "2.0.0"

  val catsKernel    = org %% "cats-kernel"    % catsVersion
  val catsCore      = org %% "cats-core"      % catsVersion
  val kindProjector = org %% "kind-projector" % "0.11.0" cross CrossVersion.full
  val simulacrum    = org %% "simulacrum"     % "1.0.0"
}

trait Nequissimus {

  private val org = "com.nequissimus"

  val sortImports = org %% "sort-imports" % "0.3.1"
}

trait ScalaMacros {

  private val org = "org.scalamacros"

  val paradise = org % "paradise" % "2.1.0" cross CrossVersion.full
}

trait Scalatest {

  private val org = "org.scalatest"

  val scalaTest = org %% "scalatest" % "3.1.0" % Test
}

object Dependencies extends Ghik with Typelevel with Nequissimus with ScalaMacros with Scalatest
