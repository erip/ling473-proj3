package edu.washington.rippeth.ling473.proj3

import scala.io.Source

object ProjectThreeDriver extends App {
  val filename = args(0)
  val lines = Source.fromFile(filename).getLines.toIterable

  val segmentedLines = new StateMachine(lines).segmentLines

  segmentedLines.toHtmlFile("rippeth.html")
}
