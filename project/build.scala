import sbt._
import Keys._

object ScalarlibBuild extends Build {
  lazy val scalaToolsVersion = settingKey[String]("2.10.0")

  lazy val exampleProject = Project("ScalaRLib", file(".")) settings(
    version       := "0.2",
    scalaVersion  := "2.10.4",
    scalaToolsVersion := "2.10",
    scalacOptions := Seq("-deprecation"),

    // Resolver for the archery R*Tree repository.
    resolvers += "bintray/meetup" at "http://dl.bintray.com/meetup/maven",

    // Choco solver (v3.2.0) repository for linear programming, since the main repository only has version 3.3.
    resolvers += "choco.repos" at "http://www.emn.fr/z-info/choco-repo/mvn/repository/",

    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,

      // Scala Graph
      "com.assembla.scala-incubator" % "graph-core_2.10" % "1.9.1",
      "org.jgrapht" % "jgrapht-core" % "0.9.0",

      // Dependency injection
      "com.escalatesoft.subcut" % "subcut_2.10" % "2.0",

      // Linear programming solver
      "choco" % "choco-solver" % "3.2.0",

      // R*Trees
      "com.meetup" % "archery_2.10" % "0.3.0",

      // Tests

      "org.scalamock" % "scalamock-scalatest-support_2.10" % "3.0.1",
      "org.scalamock" % "scalamock-core_2.10" % "3.0.1",
      "junit" % "junit" % "4.11" % "test",
      "org.specs2" % ("specs2_" + scalaToolsVersion.value) % "1.13" % "test",
      "org.scalatest" % ("scalatest_" + scalaToolsVersion.value) % "2.0.M6-SNAP8" % "test"
    ),

    unmanagedJars in Compile ++= {
      val base = baseDirectory.value
      val baseDirectories = (base / "lib")
      val customJars = (baseDirectories ** "*.jar")
      customJars.classpath
    }

  )

}