name := "divvy"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "com.twitter" % "util-app_2.10"  % "6.23.0",
  "com.twitter" % "util-core_2.10"  % "6.23.0",
  "com.google.guava" % "guava" % "18.0",
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
)
