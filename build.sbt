import AndroidKeys._

name := "Core"

organization := "info.hermesnav"

version := "1.0.0"

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-deprecation")

resolvers += "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository"

AndroidProject.androidSettings

platformName in Android := "android-15"

nativeLibrariesPath in Android <<= baseDirectory / "libs"

resolvers += ScalaToolsSnapshots

libraryDependencies := Seq(
  "net.liftweb" %% "lift-actor" % "2.5-SNAPSHOT"
)
