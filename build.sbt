lazy val `jameson` = (project in file("."))
  .aggregate(
    `jameson-core`,
    `jameson-benchmarks`,
    `jameson-examples`
  )

lazy val `jameson-core` = (project in file("core"))
  .settings(commonSettings ++ Seq(
    name := "jameson",
    // Include macro implementations in JARs
    mappings in (Compile, packageBin) ++= mappings.in(`jameson-macros`, Compile, packageBin).value,
    mappings in (Compile, packageSrc) ++= mappings.in(`jameson-macros`, Compile, packageSrc).value
  ))
  .dependsOn(`jameson-macros` % "compile-internal, test-internal")

lazy val `jameson-macros` = (project in file("macros"))
  .settings(commonSettings ++ Seq(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    ),
    publish := {},
    publishLocal := {}
  ))

lazy val `jameson-benchmarks` = (project in file("benchmarks"))
  .settings(commonSettings ++ Seq(
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "play-json" % "latest.release" % "test",
      "io.spray" %% "spray-json" % "latest.release" % "test"
    ),
    publish := {},
    publishLocal := {}
  ))
  .dependsOn(`jameson-core`)

lazy val `jameson-examples` = (project in file("examples"))
  .settings(commonSettings)
  .dependsOn(`jameson-core`)
  .settings(
    publish := {},
    publishLocal := {}
  )

def commonSettings = Seq(
  version := {
    val cmd = Array("git", "describe", "--tags", "--always", "--dirty")
    val proc = java.lang.Runtime.getRuntime.exec(cmd)
    proc.waitFor()
    IO.readStream(proc.getInputStream, java.nio.charset.Charset.defaultCharset()).trim()
  },
  organization := "jameson",
  scalaVersion := "2.12.4",
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
  ),
  libraryDependencies ++= Seq(
    //"net.liftweb"   %% "lift-json" % "2.6.+" % "test",
    "org.scalatest" %% "scalatest" % "3.0.+" % "test"
  )
)
