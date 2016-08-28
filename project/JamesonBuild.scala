import sbt._
import sbt.Keys._

object JamesonBuild extends Build {
  lazy val `jameson` = (project in file("."))
    .aggregate(`jameson-core`, `jameson-examples`)

  lazy val `jameson-core` = (project in file("core"))
    .settings(commonSettings)
    .settings(
      name := "jameson",
      libraryDependencies ++= Seq(
        "net.liftweb"   %% "lift-json" % "2.6.+" % "test",
        "org.scalatest" %% "scalatest" % "3.0.+" % "test"
      ),
      // Include macro implementations in JARs
      mappings in (Compile, packageBin) ++= mappings.in(`jameson-macros`, Compile, packageBin).value,
      mappings in (Compile, packageSrc) ++= mappings.in(`jameson-macros`, Compile, packageSrc).value
    )
    .dependsOn(`jameson-macros`)

  lazy val `jameson-macros` = (project in file("macros"))
    .settings(commonSettings)
    .settings(
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value
      )
    )

  lazy val `jameson-examples` = (project in file("examples"))
    .settings(commonSettings)
    .dependsOn(`jameson-core`)
    .settings(
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "3.0.+" % "test"
      )
    )

  def commonSettings = List(
    version := shell("git", "describe", "--tags", "--always", "--dirty").trim(),
    organization := "jameson",
    scalaVersion := "2.11.8",
    scalacOptions := Seq(
      "-encoding", "UTF-8",
      "-deprecation",
      "-feature",
      "-target:jvm-1.8",
      "-Xlint",
      "-Ywarn-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-inaccessible",
      "-Ywarn-nullary-override",
      "-Ywarn-nullary-unit",
      "-Ywarn-numeric-widen",
      "-Ywarn-unused",
      "-Ywarn-unused-import"
    )
  )

  def shell(cmd: String, args: String*): String = {
    val proc = java.lang.Runtime.getRuntime.exec((cmd +: args).toArray)
    proc.waitFor()
    IO.readStream(proc.getInputStream, java.nio.charset.Charset.defaultCharset())
  }
}
