libraryDependencies  ++= Seq(
            // other dependencies here
            "org.scalanlp" %% "breeze" % "0.10",
            // native libraries are not included by default. add this if you want them (as of 0.7)
            // native libraries greatly improve performance, but increase jar sizes.
            "org.scalanlp" %% "breeze-natives" % "0.10"
)

resolvers ++= Seq(
            // other resolvers here
            "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
            "jgit-repo" at "http://download.eclipse.org/jgit/maven"
)

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.6.4")

lazy val root = (project in file(".")).
	settings(
		name := "squire"
	)
