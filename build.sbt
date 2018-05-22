lazy val xdrGen = project.in(file(".")).
  settings(
    organization := "org.strllar",
    scalaVersion := "2.12.6",
    version := "0.2-SNAPSHOT",
    libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3",
    libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.4",
    libraryDependencies += "org.scalameta" %% "scalameta" % "3.7.3",
    libraryDependencies += "org.scalameta" %% "contrib" % "3.7.3",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.5" % "test",
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck)
  )

lazy val xdrbase = crossProject.crossType(CrossType.Full).in(file(".")).
   settings(
     organization := "org.strllar",
     scalaVersion := "2.12.6",
     libraryDependencies += "org.scalameta" %% "scalameta" % "3.7.3",
     libraryDependencies += "org.scalameta" %% "contrib" % "3.7.3",
     libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.5" % "test",
   )

lazy val targetJVM = xdrbase.jvm
lazy val targetJS = xdrbase.js
