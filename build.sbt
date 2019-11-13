name := "BinCalc"
scalaVersion := "2.13.1"

version := "0.1"
scalacOptions += "-Ypartial-unification"


libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3",
"org.typelevel" %% "cats-core" % "2.0.0-M4"
)
