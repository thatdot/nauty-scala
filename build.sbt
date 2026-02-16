// Version is managed by sbt-dynver from git tags
ThisBuild / scalaVersion := "2.13.12"
ThisBuild / organization := "com.thatdot"

// POM metadata required by Maven Central
ThisBuild / homepage := Some(url("https://github.com/thatdot/nauty-scala"))
ThisBuild / licenses := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/thatdot/nauty-scala"),
    "scm:git@github.com:thatdot/nauty-scala.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id = "thatdot",
    name = "thatdot",
    email = "oss@thatdot.com",
    url = url("https://thatdot.com")
  )
)

// Maven Central Portal (new Sonatype)
ThisBuild / sonatypeCredentialHost := "central.sonatype.com"

// Version scheme for eviction warnings
ThisBuild / versionScheme := Some("early-semver")

lazy val root = (project in file("."))
  .settings(
    name := "nauty-scala",
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
