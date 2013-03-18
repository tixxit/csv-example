import sbt._
import sbt.Keys._

object ScalaTOBuild extends Build {
  override lazy val settings = super.settings ++ Seq(
    name := "csv",
    organization := "net.tixxit",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := "2.9.2",
    // scalaBinaryVersion := "2.10",

    licenses := Seq("BSD-style" -> url("http://opensource.org/licenses/MIT")),
    homepage := Some(url("http://tixxit.net/")),

    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "1.2.4",
      "org.scalaz" %% "scalaz-core" % "7.0.0-M8"
    ),

    scalacOptions ++= Seq(
      //"-Yinline-warnings",
      "-Xlog-implicits",
      "-deprecation",
      "-unchecked"
      // "-optimize",
      //"-feature"
    )
  )

  lazy val csv = Project("csv", file("csv"))
  lazy val examples = Project("examples", file("examples")).dependsOn(csv)
}
