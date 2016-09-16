name := "FunctionalProgrammingInScala"
scalaVersion := "2.11.7"


libraryDependencies ++= Seq(
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test,
  "org.scalaz" %% "scalaz-core" % "7.1.3",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.typelevel" %% "cats" % "0.4.0",
  "com.storm-enroute" %% "scalameter-core" % "0.6",
  "com.thoughtworks.each" % "each_2.11" % "0.3.0",
  "com.typesafe.akka" %% "akka-actor" % "2.4.4"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
