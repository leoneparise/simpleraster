import sbt._
import sbt.Keys._

object RasterBuild extends Build {
  lazy val project = Project("raster", file(".")) settings(
    organization := "br.com.climadata",

    name := "raster",

    version := "1.0-SNAPSHOT",

    scalaVersion := "2.10.0",

    scalacOptions ++= Seq("-deprecation", "-unchecked", "-optimize", "-feature"),

    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % "2.10.0",
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

  lazy val benchmark:Project = Project("benchmark", file("benchmark")).
    settings(benchmarkSettings: _*).
    dependsOn(project)

  def benchmarkSettings = Seq(
    scalaVersion := "2.10.0",

    // raise memory limits here if necessary
    javaOptions += "-Xmx8G",

    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % "r09",
      "com.google.code.java-allocation-instrumenter" % "java-allocation-instrumenter" % "2.0",
      "com.google.code.caliper" % "caliper" % "1.0-SNAPSHOT" 
          from "http://plastic-idolatry.com/jars/caliper-1.0-SNAPSHOT.jar",
      "com.google.code.gson" % "gson" % "1.7.1",
      "com.typesafe.akka" %% "akka-kernel" % "2.1.0",
      "com.typesafe.akka" %% "akka-remote" % "2.1.0",
      "com.typesafe.akka" %% "akka-actor" % "2.1.0"
    ),

    // enable forking in both run and test
    fork := true,

    // set the correct working directory for acces to resources in test

    runner in Compile in run <<= (thisProject, 
                                  taskTemporaryDirectory, 
                                  scalaInstance, 
                                  baseDirectory, 
                                  javaOptions, 
                                  outputStrategy, 
                                  javaHome, 
                                  connectInput) map {
      (tp, tmp, si, base, options, strategy, javaHomeDir, connectIn) =>
      new BenchmarkRunner(tp.id, ForkOptions(scalaJars = si.jars,
                                             javaHome = javaHomeDir,
                                             connectInput = connectIn,
                                             outputStrategy = strategy,
                                             runJVMOptions = options,
                                             workingDirectory = Some(base)))
    }
  )
}

class BenchmarkRunner(subproject: String, config: ForkScalaRun) extends sbt.ScalaRun {
  def run(mainClass: String, classpath: Seq[File], options: Seq[String], log: Logger): Option[String] = {
    log.info("Running " + subproject + " " + mainClass + " " + options.mkString(" "))

    val javaOptions = classpathOption(classpath) ::: mainClass :: options.toList
    val strategy = config.outputStrategy getOrElse LoggedOutput(log)
    val jvmopts = config.runJVMOptions ++ javaOptions
    val process =  Fork.java.fork(config.javaHome,
                                  jvmopts,
                                  config.workingDirectory,
                                  Map.empty,
                                  config.connectInput,
                                  strategy)
    def cancel() = {
      log.warn("Run canceled.")
      process.destroy()
      1
    }
    val exitCode = try process.exitValue() catch { case e: InterruptedException => cancel() }
    processExitCode(exitCode, "runner")
  }
  private def classpathOption(classpath: Seq[File]) = "-classpath" :: Path.makeString(classpath) :: Nil
  private def processExitCode(exitCode: Int, label: String) = {
    if(exitCode == 0) None
    else Some("Nonzero exit code returned from " + label + ": " + exitCode)
  }
}
