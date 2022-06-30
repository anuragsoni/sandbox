ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.8"

val finagleVersion = "22.4.0"

val finagleDependencies = Seq(
  "com.twitter" %% "finagle-core",
  "com.twitter" %% "finagle-stats",
  "com.twitter" %% "finagle-netty4"
).map(_ % finagleVersion)

lazy val root = (project in file("."))
  .settings(
    name := "finagle-echo",
    libraryDependencies ++= finagleDependencies
  )
