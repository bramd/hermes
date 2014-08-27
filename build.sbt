import android.Keys._

name := "Core"

organization := "info.hermesnav"

version := "1.0.0"

versionCode := Some(0)

scalaVersion := "2.10.4"

scalacOptions ++= Seq("-deprecation", "-feature", "-language:implicitConversions,postfixOps", "-target:jvm-1.7")

javacOptions ++= Seq("-source", "1.7", "-target", "1.7")

libraryDependencies := Seq(
  "com.google.android.gms" % "play-services" % "4.4.52",
  "org.scaloid" %% "scaloid" % "2.3-8",
  "ch.acra" % "acra" % "4.5.0",
  "com.graphhopper" % "graphhopper" % "0.3",
  "org.mapsforge" % "mapsforge-map-reader" % "0.4.3"
)

proguardOptions in Android += """
  -ignorewarnings
  -keep class jsqlite.** { *; }
  -keep class info.hermesnav.** { *; }
  -keep class com.graphhopper.** { *; }
  -keep class org.mapsforge.** { *; }
"""
