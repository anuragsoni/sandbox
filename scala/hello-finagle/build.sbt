ThisBuild / scalaVersion := "2.13.8"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

val finagleVersion = "22.4.0"

val finagleDependencies = Seq(
  "com.twitter" %% "finagle-http",
  "com.twitter" %% "finagle-stats",
  "com.twitter" %% "twitter-server"
).map(_ % finagleVersion)

val loggingDependencies = Seq(
  "ch.qos.logback" % "logback-classic" % "1.2.11"
)

lazy val root = (project in file("."))
  .settings(
    name := "hello-finagle",
    libraryDependencies ++= finagleDependencies ++ loggingDependencies
  )
