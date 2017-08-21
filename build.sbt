lazy val `project-three` = (project in file("."))
  .settings(
    name := "project3",
    scalaVersion := "2.12.2",
    version := "0.0.1-SNAPSHOT",
    libraryDependencies ++= Dependencies.projectThreeDependencies,
    mainClass in Compile := Some("edu.washington.rippeth.ling473.proj3.ProjectThreeDriver"),
    fork in run := true,
    javaOptions in Compile := Seq("-Dfile.encoding=UTF-8"),
    logLevel in run := Level.Warn,
    showSuccess := false
  )
