import sbt._
import sbt.Keys._

object JamesonBuild extends Build {
  lazy val `core` = (project in file("core"))
    .settings(
      scalaVersion := "2.11.8",
      scalacOptions := Seq(
        "-encoding", "UTF-8",
        "-Ywarn-adapted-args",
        "-Ywarn-dead-code",
        "-Ywarn-inaccessible",
        "-Ywarn-nullary-override",
        "-Ywarn-nullary-unit",
        "-Ywarn-numeric-widen",
        "-Ywarn-unused",
        "-Ywarn-unused-import",
        "-Ywarn-value-discard"
      ),
      libraryDependencies <++= (scalaVersion) { (sv) =>
        Seq(
          "net.liftweb"   %% "lift-json" % "2.6.+" % "test",
          "org.scalatest" %% "scalatest" % "3.0.+" % "test"
        )
      }
    )
}
