name := "squire"

organization := "org.jrj-d"

scalaVersion := "2.12.3"

libraryDependencies  ++= Seq(
            // other dependencies here
            "org.scalanlp" %% "breeze" % "0.13.2",
            // native libraries are not included by default. add this if you want them (as of 0.7)
            // native libraries greatly improve performance, but increase jar sizes.
            "org.scalanlp" %% "breeze-natives" % "0.13.2",
            "org.scalatest" % "scalatest_2.12" % "3.0.4" % "test",
            "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
            "org.typelevel" %% "cats-core" % "1.0.0-RC1"


)

resolvers ++= Seq(
            // other resolvers here
            "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

//// scalastyle for main code
//
//lazy val compileScalaStyle = taskKey[Unit]("compileScalaStyle")
//
//compileScalaStyle := {
//  scalastyle.in(Compile).toTask("").value
//}
//
//(compile in Compile) := ((compile in Compile) dependsOn compileScalaStyle).value
//
//// scalastyle for tests
//
//(scalastyleConfig in Test) := baseDirectory.value / "scalastyle-test-config.xml"
//
//lazy val testScalaStyle = taskKey[Unit]("testScalaStyle")
//
//testScalaStyle := {
//  scalastyle.in(Test).toTask("").value
//}
//
//(test in Test) := ((test in Test) dependsOn testScalaStyle).value
