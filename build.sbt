lazy val app = project.in(file("."))
  .settings(
    scalaVersion := "3.4.3",
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "toolkit" % "0.1.7",
      // for parsing
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
      // unit testing library
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
      // for JSON marshalling/unmarshalling
      "io.spray" %%  "spray-json" % "1.3.6"
    )
  )
