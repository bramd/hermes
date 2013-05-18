import org.scalasbt.androidplugin._
import AndroidKeys._


name := "Core"

organization := "info.hermesnav"

version := "1.0.0"

scalaVersion := "2.10.1"

scalacOptions ++= Seq("-deprecation", "-feature", "-language:implicitConversions,postfixOps")

AndroidProject.androidSettings

platformName in Android := "android-17"

nativeLibrariesPath in Android <<= baseDirectory / "libs"

proguardOption in Android := """
  -keep class scala.collection.SeqLike { *; }
"""

addArtifact(Artifact("google-play-services", "apklib", "apklib"), apklibPackage in Android).settings

libraryDependencies := Seq(
  "org.scaloid" % "scaloid" % "1.1_8_2.10"
)
