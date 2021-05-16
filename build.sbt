import sbt.Keys._
import sbtassembly.AssemblyKeys.{assembly, mergeStrategy}

course := "parprog1"
assignment := "barneshut"

scalaVersion := "2.13.0"
scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")
libraryDependencies ++= Seq(
  "com.storm-enroute" %% "scalameter-core" % "0.19",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0",
  "com.novocode" % "junit-interface" % "0.11" % Test
)


testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")

lazy val commonSettings = Seq(

)

assemblyMergeStrategy in assembly := {
  case x if x.startsWith("META-INF") => MergeStrategy.discard // Bumf
  case x if x.endsWith(".html") => MergeStrategy.discard // More bumf
  case x if x.contains("slf4j-api") => MergeStrategy.last
  case x if x.contains("org/cyberneko/html") => MergeStrategy.first
  case PathList("com", "esotericsoftware", xs@_ *) => MergeStrategy.last // For Log$Logger.class
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

lazy val app = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    assembly / mainClass := Some("StartSimulation"),
    assembly / assemblyJarName := "Simulation.jar"

    // more settings here ...
  )
