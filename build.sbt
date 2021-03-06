name := "aktorrent"

version := "1.0.1"

scalaVersion := "2.11.8"

scalacOptions ++= Seq(
  "-target:jvm-1.8",
  "-encoding", "UTF-8",
  "-unchecked",
  "-deprecation",
  "-Xfuture",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused"
)

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.3"
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"
libraryDependencies += "io.netty" % "netty-all" % "4.1.11.Final"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.2",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.2" % Test
)


// Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.0" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0-M15"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0-M15" % "test"
