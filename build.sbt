scalaVersion := "3.6.2"

scalacOptions ++= Seq("-Wall", "-Wconf:any:e", "-Wunused:all", "-feature", "-deprecation", "-unchecked", "-Yexplicit-nulls")

testFrameworks += new TestFramework("munit.Framework")

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "os-lib" % "0.11.3" ,
  "com.lihaoyi" %% "sourcecode" % "0.4.2" ,
  "com.lihaoyi" %% "upickle" % "4.0.2" 
)

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "1.0.3" % Test,
  "com.lihaoyi" %% "os-lib" % "0.11.3" % Test,
  "com.lihaoyi" %% "sourcecode" % "0.4.2" % Test,
  "com.lihaoyi" %% "upickle" % "4.0.2" % Test
)

