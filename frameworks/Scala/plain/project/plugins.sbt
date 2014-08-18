logLevel := Level.Warn

resolvers += Resolver.url("artifactory", url("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

resolvers += "spray repo" at "http://repo.spray.io"

addSbtPlugin("com.github.retronym" % "sbt-onejar" % "0.8")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.1.2")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.1.3")

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.0.1")

addSbtPlugin("me.lessis" % "ls-sbt" % "0.1.2")

addSbtPlugin("io.spray" % "sbt-revolver" % "0.6.2")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.8.8")

