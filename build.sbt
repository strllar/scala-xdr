lazy val xdrGen = project.in(file(".")).
  settings(
    organization := "org.strllar",
    scalaVersion := "2.11.7",
    version := "0.1-SNAPSHOT",
    libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.0",
    libraryDependencies +=  "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )

lazy val xdrbase = crossProject.crossType(CrossType.Full).in(file(".")).
   settings(
     organization := "org.strllar",
     scalaVersion := "2.10.5",
     libraryDependencies +=  "org.scala-lang" % "scala-reflect" % scalaVersion.value
   )

lazy val targetJVM = xdrbase.jvm
lazy val targetJS = xdrbase.js
