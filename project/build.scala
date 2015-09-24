import sbt._
import Keys._

object ScalarlibBuild extends Build {
  lazy val scalaToolsVersion = settingKey[String]("2.10.0")

  lazy val exampleProject = Project("ScalaRLib", file(".")) settings(
    version           := "1.0",
    scalaVersion      := "2.11.7",
    scalaToolsVersion := "2.10",
    scalacOptions     := Seq("-deprecation"),
    organization      := "net.cyndeline",
    name              := "scalarlib",

    // Needed to access user classes
    resolvers += Resolver.mavenLocal,

    // Resolver for the archery R*Tree repository.
    resolvers += "bintray/meetup" at "http://dl.bintray.com/meetup/maven",

    // Choco solver (v3.2.0) repository for linear programming, since the main repository only has version 3.3.
    resolvers += "choco.repos" at "http://www.emn.fr/z-info/choco-repo/mvn/repository/",

    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,

      // User libraries
      "net.cyndeline" % "rlcommon_2.11" % "1.0",
      "net.cyndeline" % "rlgraph_2.11" % "1.0",

      // Scala Graph
      "com.assembla.scala-incubator" % "graph-core_2.11" % "1.9.4",
      "org.jgrapht" % "jgrapht-core" % "0.9.0",

      // Dependency injection
      "com.escalatesoft.subcut" % "subcut_2.10" % "2.0",

      // Tests

      "org.scalamock" %% "scalamock-scalatest-support" % "3.2.2" % "test",
      "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test",
      "junit" % "junit" % "4.11" % "test"
    ),

    unmanagedJars in Compile ++= {
      val base = baseDirectory.value
      val baseDirectories = (base / "lib")
      val customJars = (baseDirectories ** "*.jar")
      customJars.classpath
    }

  )

}