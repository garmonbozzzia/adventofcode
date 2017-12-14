name := "adventofcode"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.6.0" % "test"
testFrameworks += new TestFramework("utest.runner.Framework")

//libraryDependencies += "org.gbz" % "utils_2.12" % "0.2.1-SNAPSHOT"
libraryDependencies += "com.typesafe.akka" %% "akka-http" % "10.0.10"
libraryDependencies += "com.lihaoyi" %% "fastparse" % "1.0.0"

lazy val core = RootProject(file("../utils"))
val main = Project(id = "application", base = file(".")).dependsOn(core)