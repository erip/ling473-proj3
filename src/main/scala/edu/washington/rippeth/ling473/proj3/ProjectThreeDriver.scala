package edu.washington.rippeth.ling473.proj3

import scala.io.Source

object ProjectThreeDriver extends App {
  val lines = Source.fromFile("fsm-input.utf8.txt").getLines.toIterable

  val segmentedLines = new StateMachine(lines).segmentLines

  segmentedLines.toHtmlFile("output.html")
}
