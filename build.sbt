lazy val xdrGen = project.in(file(".")).
  settings(
    organization := "org.strllar",
    scalaVersion := "2.12.3",
    version := "0.2-SNAPSHOT",
    libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2",
    libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.4",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.3" % "test",
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck)
  )

lazy val xdrbase = crossProject.crossType(CrossType.Full).in(file(".")).
   settings(
     organization := "org.strllar",
     scalaVersion := "2.12.3",
     libraryDependencies +=  "org.scalameta" %% "scalameta" % "1.8.0",
     libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.3" % "test",
     resolvers += Resolver.sonatypeRepo("releases"),
     addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.full)
   )

lazy val targetJVM = xdrbase.jvm
lazy val targetJS = xdrbase.js
