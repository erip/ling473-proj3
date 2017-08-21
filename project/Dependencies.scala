import sbt._

object Dependencies {
  private val scalaTestVersion = "3.0.1"

  private val scalatic = "org.scalactic" %% "scalactic" % scalaTestVersion
  private val scalatest = "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
  private val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2"
  private val logbackClassic = "ch.qos.logback" % "logback-classic" % "1.2.3"

  val projectThreeDependencies: Seq[ModuleID] = Seq(
    scalatic,
    scalatest,
    scalaLogging,
    logbackClassic
  )
}