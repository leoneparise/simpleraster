unmanagedSourceDirectories in Compile <<= Seq(scalaSource in Compile).join

unmanagedSourceDirectories in Test <<= Seq(scalaSource in Test).join
