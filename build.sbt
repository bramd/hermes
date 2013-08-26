import sbtandroid._

androidDefaults

name := "Core"

organization := "info.hermesnav"

version := "1.0.0"

versionCode := 0

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-deprecation", "-feature", "-language:implicitConversions,postfixOps", "-target:jvm-1.6")

javacOptions ++= Seq("-source", "1.6", "-target", "1.6")

platformName := "android-18"

libraryDependencies := Seq(
aarlib("com.google.android.gms" % "play-services" % "3.1.36"),
  "org.scaloid" %% "scaloid" % "2.3-8"
)
