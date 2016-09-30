name := "aktorrent"

version := "1.0.1"

scalaVersion := "2.11.8"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.3"
libraryDependencies += "org.typelevel" %% "cats" % "0.7.0"
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"

// Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.0" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0-M15"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0-M15" % "test"
