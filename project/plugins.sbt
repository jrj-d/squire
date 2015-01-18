resolvers += Classpaths.sbtPluginReleases

resolvers += Classpaths.typesafeReleases

// Add the following to have Git manage your build versions
resolvers += "jgit-repo" at "http://download.eclipse.org/jgit/maven"

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.6.4")
