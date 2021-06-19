val dottyVersion = "3.0.1-RC1-bin-20210412-69108bf-NIGHTLY"

lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc2020",
    version := "0.1.0",
    description := "Solutions for Advent Of Codes 2020",
    scalaVersion := dottyVersion,
    libraryDependencies ++= Seq(
      "junit" % "junit" % "4.13",
      "org.scala-sbt" % "test-interface" % "1.0"
    ),
    //libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
//libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"
