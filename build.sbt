name := """psug201607"""

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.8"


// Change this to another test framework if you prefer
libraryDependencies ++= Seq(
  "org.typelevel"              %% "discipline"                % "0.5",
  "org.typelevel"              %% "cats"                      % "0.6.0",
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.0-RC1",
  "org.scalactic"              %% "scalactic"                 % "3.0.0-M16-SNAP4",
  "org.scalatest"              %% "scalatest"                 % "3.0.0-M16-SNAP4" % "test"
)

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"

scalacOptions ++= Seq(
  "-deprecation",
  "-explaintypes",
  "-feature",
  "-language:_",
  //"-print",
  "-unchecked",
  //"-verbose",
  //"-Xdev",
  "-Xexperimental",
  //"-Xfatal-warnings",
  "-Xfuture",
  //"-Xgenerate-phase-graph", "graph.dot",
  "-Xlint:_",
  //"-Xlog-implicit-conversions",
  //"-Xlog-implicits",
  "-Xprint-icode:icode",
  "-Ybackend:GenBCode",
  "-Ybreak-cycles",
  "-Yclosure-elim",
  "-Yconst-opt",
  "-Ydead-code",
  "-Ydelambdafy:method",
  "-Ygen-asmp", "asmp",
  "-Ygen-javap", "javap",
  "-Yinfer-argument-types",
  "-Yinline",
  "-Yinline-handlers",
  "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Yrecursion" , "0",
  //"-Ystatistics:_",
  "-Ywarn-inaccessible",
  "-Ywarn-infer-any",
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard"
)