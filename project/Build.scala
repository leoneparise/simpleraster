import sbt._
import sbt.Keys._

object SebalBuild extends Build {
  lazy val project = Project("raster", file(".")) settings(
    organization := "br.com.climadata",

    name := "raster",

    version := "1.0-SNAPSHOT",

    scalaVersion := "2.10.0",

    scalacOptions ++= Seq("-deprecation", "-unchecked", "-optimize"),

    parallelExecution := false,

    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2" % "1.13" % "test"
    ),

    resolvers ++= Seq(
      "maven2 dev repository" at "http://download.java.net/maven/2",
      "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/",
      "sonatypeSnapshots" at "http://oss.sonatype.org/content/repositories/snapshots"
    ),

    // enable forking in test
    fork in test := true
  )
}