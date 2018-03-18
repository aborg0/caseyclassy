addSbtPlugin("org.scala-js"         % "sbt-scalajs"              % "0.6.22")
addSbtPlugin("org.portable-scala"   % "sbt-crossproject"         % "0.3.1")  // (1)
addSbtPlugin("org.portable-scala"   % "sbt-scalajs-crossproject" % "0.3.1")  // (2)
addSbtPlugin("org.scala-native"     % "sbt-scala-native"         % "0.3.6")  // (3)
addSbtPlugin("org.tpolecat"         % "tut-plugin"               % "0.6.3")

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
addSbtPlugin("com.artima.supersafe" % "sbtplugin"                % "1.1.5")
addSbtPlugin("org.planet42"         % "laika-sbt"                % "0.7.5")
addSbtPlugin("org.scoverage"        % "sbt-scoverage"            % "1.5.1")
addSbtPlugin("org.scoverage"        % "sbt-coveralls"            % "1.2.2")
addSbtPlugin("com.geirsson"         % "sbt-scalafmt"             % "1.4.0")