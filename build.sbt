ThisBuild / scalaVersion := "2.13.10"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "TicTacToe",
    trapExit := false,
    libraryDependencies ++= Seq(
      Dependencies.refined,
      Dependencies.scalaTest % Test
    )
  )
