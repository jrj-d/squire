scalaVersion := "2.10.4"

libraryDependencies  ++= Seq(
            // other dependencies here
            "org.scalanlp" %% "breeze" % "0.10",
            // native libraries are not included by default. add this if you want them (as of 0.7)
            // native libraries greatly improve performance, but increase jar sizes.
            "org.scalanlp" %% "breeze-natives" % "0.10",
            "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
)

resolvers ++= Seq(
            // other resolvers here
            "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

ScoverageKeys.excludedPackages in ScoverageCompile := "<empty>;squire.debug.*"

instrumentSettings

CoverallsPlugin.coverallsSettings

lazy val root = (project in file(".")).
	settings(
		name := "squire"
	)
