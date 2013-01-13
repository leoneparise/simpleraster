// Comment to get more information during initialization
logLevel := Level.Warn

// The Typesafe repository 
resolvers += Classpaths.typesafeResolver

// Use the Play sbt plugin for Play projects
addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.1.1")

addSbtPlugin("com.typesafe.sbt" % "sbt-start-script" % "0.6.0")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.2.0")

