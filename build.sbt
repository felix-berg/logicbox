val akkaVersion = "2.7.0"
val akkaHttpVersion = "10.5.0"

lazy val app = project.in(file("."))
  .settings(
    scalaVersion := "3.3.3",
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "toolkit" % "0.1.7",

      // for parsing
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",

      // unit testing library
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
      
      // for HTTP
      "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
      "com.typesafe.akka" %% "akka-stream-typed" % akkaVersion,
      "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,

      // for JSON marshalling/unmarshalling
      "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion
    )
  )
