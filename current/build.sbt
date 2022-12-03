ThisBuild / organization := "com.example"
ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file(".")).settings(
  name := "current",
  libraryDependencies ++= Seq("co.fs2" %% "fs2-core" % "3.4.0",
    "co.fs2" %% "fs2-io" % "3.4.0",
    "org.scalameta" %% "munit" % "0.7.29" % Test,
    "org.typelevel" %% "munit-cats-effect" % "2.0.0-M3" % Test
  )
)
