//import sbtcrossproject.{crossProject, CrossType}

//enablePluginsugins(TutPlugin)

//enablePlugins(LaikaPlugin)

val sharedSettings = Seq(
  scalaVersion := "3.3.0",
//  scalacOptions ++= Seq( // from: https://tpolecat.github.io/2017/04/25/scalac-flags.html
//    "-deprecation", // Emit warning and location for usages of deprecated APIs.
//    "-encoding",
//    "utf-8", // Specify character encoding used by source files.
//    "-explaintypes", // Explain type errors in more detail.
//    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
//    "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
//    "-language:experimental.macros", // Allow macro definition (besides implementation and application)
//    "-language:higherKinds", // Allow higher-kinded types
//    "-language:implicitConversions", // Allow definition of implicit functions called views
//    "-unchecked", // Enable additional warnings where generated code depends on assumptions.
//    "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
//    "-Xfatal-warnings", // Fail the compilation if there are any warnings.
//    "-Xfuture", // Turn on future language features.
//    "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
//    "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
//    "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
//    "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
//    "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
//    "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
//    "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
//    "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
//    "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
//    "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
//    "-Xlint:option-implicit", // Option.apply used implicit view.
//    "-Xlint:package-object-classes", // Class or object defined in package object.
//    "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
//    "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
//    "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
//    "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
//    "-Xlint:unsound-match", // Pattern match may not be typesafe.
//    "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
//    "-Ypartial-unification", // Enable partial unification in type constructor inference
//    "-Ywarn-dead-code", // Warn when dead code is identified.
//    "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
//    "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
//    "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
//    "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
//    "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
//    "-Ywarn-numeric-widen", // Warn when numerics are widened.
//    "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
//    "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
//    "-Ywarn-unused:locals", // Warn if a local definition is unused.
//    "-Ywarn-unused:params", // Warn if a value parameter is unused.
//    "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
//    "-Ywarn-unused:privates", // Warn if a private member is unused.
//    "-Ywarn-value-discard" // Warn when non-Unit expression results are unused.
//  ),
//  Compile / console / scalacOptions --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings")
)

name := "caseyclassy"

organization := "com.github.aborg0"

version := "0.1"

scalaVersion := "3.3.0"

startYear := Some(2018)

description := "Parser for case class toStrings"

scalafmtCheck := true

//lazy val caseyClassy =
// (6) select supported platforms
//  crossProject(JSPlatform, JVMPlatform /*, NativePlatform*/)
//    .withoutSuffixFor(JVMPlatform)
//    .crossType(CrossType.Pure) // [Pure, Full, Dummy], default: CrossType.Full
//    .settings(sharedSettings)
//    .jsSettings(/* ... */) // defined in sbt-scalajs-crossproject
//    .jvmSettings(
////    osgiSettings,
////    libraryDependencies += "org.osgi" % "org.osgi.core" % "4.3.0" % "provided",
////    OsgiKeys.bundleSymbolicName := "com.github.aborg0.caseyclassy",
////    OsgiKeys.exportPackage := Seq("com.github.aborg0.caseyclassy"),
//    /* ... */)
// (7) configure Scala-Native settings
//    .nativeSettings(/* ... */) // defined in sbt-scala-native

//lazy val caseyClassyJS = caseyClassy.js
//lazy val caseyClassyJVM = caseyClassy.jvm//.enablePlugins(SbtOsgi)

//lazy val caseyClassyNative = caseyClassy.native

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.16"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.16" % "test"

//resolvers ++= Seq(
//  Resolver.sonatypeRepo("releases"),
//  Resolver.sonatypeRepo("snapshots")
//)

//libraryDependencies ++= Seq(
//  "com.chuusai" %%% "shapeless" % "2.3.3"
//)

libraryDependencies += "com.lihaoyi" %% "fastparse" % "3.0.1"

//libraryDependencies += "com.propensive" %%% "magnolia" % "0.16.0"