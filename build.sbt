ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.12"
ThisBuild / organization := "com.thatdot"

lazy val root = (project in file("."))
  .settings(
    name := "scala-nauty",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.17" % Test,
      "org.scalatestplus" %% "scalacheck-1-17" % "3.2.17.0" % Test,
      "org.scalacheck" %% "scalacheck" % "1.17.0" % Test
    ),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-opt:l:inline",
      "-opt-inline-from:**",
      "-Xlint:_"
    ),
    // Enable optimizer for performance
    Compile / compile / scalacOptions ++= Seq(
      "-opt:l:method",
      "-opt:l:inline"
    )
  )
