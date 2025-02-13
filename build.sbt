lazy val app = project.in(file("."))
  .settings(
    scalaVersion := "3.3.3",
    libraryDependencies += "org.scala-lang" %% "toolkit" % "0.1.7",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  )
