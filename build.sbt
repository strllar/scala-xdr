lazy val xdrGen = project.in(file(".")).
  settings(
    organization := "org.strllar",
    scalaVersion := "2.11.8",
    version := "0.2-SNAPSHOT",
    libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2",
    libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.3",
    resolvers += Resolver.url("inthenow-releases", url("http://dl.bintray.com/inthenow/releases"))(Resolver.ivyStylePatterns),
    libraryDependencies += "com.github.inthenow" %% "zcheck" % "0.6.2",
    //libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.1" % "test"
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck)
  )

lazy val xdrbase = crossProject.crossType(CrossType.Full).in(file(".")).
   settings(
     organization := "org.strllar",
     scalaVersion := "2.11.8",
     libraryDependencies +=  "org.scalameta" %% "scalameta" % "1.3.0",
     libraryDependencies += "com.github.inthenow" %% "zcheck" % "0.6.2",
     //libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.1" % "test"
     testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck),
     resolvers += Resolver.sonatypeRepo("releases"),
     addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M5" cross CrossVersion.full)
   )

lazy val targetJVM = xdrbase.jvm
lazy val targetJS = xdrbase.js
