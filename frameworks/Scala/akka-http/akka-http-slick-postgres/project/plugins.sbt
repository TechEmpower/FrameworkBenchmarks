addSbtPlugin("com.typesafe.sbt"   % "sbt-git"             % "1.0.0")
addSbtPlugin("com.typesafe.sbt"   % "sbt-native-packager" % "1.3.16")
addSbtPlugin("com.lucidchart"     % "sbt-scalafmt"        % "1.15")
addSbtPlugin("org.wartremover"    % "sbt-wartremover"     % "3.3.2")
addSbtPlugin("io.get-coursier"    % "sbt-coursier"        % "1.0.3")

libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.7.25" // Needed by sbt-git
