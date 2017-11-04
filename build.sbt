name := "divvy"

scalaVersion in ThisBuild := "2.10.4"

lazy val root = project.in(file(".")).
  aggregate(divvyJS, divvyJVM).
  settings(
    publish := {},
    publishLocal := {}
  )


lazy val divvy = crossProject.in(file(".")).
  settings(
    name := "divvy",
    version := "0.1-SNAPSHOT"
  ).
  jvmSettings(
    libraryDependencies += "com.twitter" % "util-app_2.10"  % "6.23.0",
    libraryDependencies += "com.twitter" % "util-core_2.10"  % "6.23.0",
    libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided"
  ).
  jsSettings(
    // Add JS-specific settings here
    skip in packageJSDependencies := false,
    scalaJSUseMainModuleInitializer := true,
    jsDependencies += "org.webjars" % "jquery" % "2.1.4" / "2.1.4/jquery.js",
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1",
    libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.9.1"
  )

lazy val divvyJVM = divvy.jvm
lazy val divvyJS = divvy.js
