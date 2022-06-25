import Dependencies._

ThisBuild / scalaVersion := "2.13.8"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

val finagleVersion = "22.4.0"

val finagleDependencies = Seq("com.twitter" %% "finagle-http" % finagleVersion)

lazy val root = (project in file("."))
  .settings(
    name := "hello-finagle",
    libraryDependencies ++= finagleDependencies
  )
