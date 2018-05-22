lazy val xdrGen = project.in(file(".")).
  settings(
    organization := "org.strllar",
    version := "0.2-SNAPSHOT",
    scalaVersion := "2.12.6",
    scalacOptions += "-Ypartial-unification",
    libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1",
    libraryDependencies += "org.typelevel" %% "cats-free" % "1.0.1",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "1.0.0-RC",
    libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3",
    libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.4",
    libraryDependencies += "org.scalameta" %% "scalameta" % "3.7.3",
    libraryDependencies += "org.scalameta" %% "contrib" % "3.7.3",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.5" % "test",
    libraryDependencies += "org.typelevel" %% "cats-laws" % "1.0.1" % "test",
    libraryDependencies += "org.typelevel" %% "cats-testkit" % "1.0.1" % "test",
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck)
  )

lazy val xdrbase = crossProject.crossType(CrossType.Full).in(file(".")).
   settings(
     organization := "org.strllar",
     scalaVersion := "2.12.6",
     scalacOptions += "-Ypartial-unification",
     libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1",
     libraryDependencies += "org.typelevel" %% "cats-free" % "1.0.1",
     libraryDependencies += "org.typelevel" %% "cats-effect" % "1.0.0-RC",
     libraryDependencies += "org.scalameta" %% "scalameta" % "3.7.3",
     libraryDependencies += "org.scalameta" %% "contrib" % "3.7.3",
     libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.5" % "test",
     libraryDependencies += "org.typelevel" %% "cats-laws" % "1.0.1" % "test",
     libraryDependencies += "org.typelevel" %% "cats-testkit" % "1.0.1" % "test"
   )

lazy val targetJVM = xdrbase.jvm
lazy val targetJS = xdrbase.js
