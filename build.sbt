ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.rtjfarrimond"
ThisBuild / organizationName := "rtjfarrimond"

lazy val root = (project in file("."))
  .settings(
    name := "minesweeper",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.0.0",
      "org.typelevel" %% "cats-effect" % "2.1.2",
      "org.scalatest" %% "scalatest" % "3.0.8" % Test
    )
  )
