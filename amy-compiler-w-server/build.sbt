lazy val amyc = (project in file("."))
  .disablePlugins(plugins.JUnitXmlReportPlugin)
  .enablePlugins(StudentPlugin)
  .settings(
    name := "amyc",

    version := "1.7",
    organization := "ch.epfl.lara",
    scalaVersion := "2.12.10",

    scalaSource in Compile := baseDirectory.value / "src",
    scalacOptions ++= Seq("-feature"),

    scalaSource in Test := baseDirectory.value / "test" / "scala",
    parallelExecution in Test := false,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.10.0",
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-v"),

    // solves RPC errors
    scalacOptions += "-Xmixin-force-forwarders:false",

    assemblyJarName in assembly := "../../../amy-client/launcher/amy-server.jar",
    test in assembly := {},
    mainClass in assembly := Some("amyc.Main")
  )
