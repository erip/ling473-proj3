package edu.washington.rippeth.ling473.proj3

import java.io.{BufferedWriter, File, FileWriter}

class SegmentedLines(l: Iterable[String]) {

  /** The trimmed input lines in case an extra space appears at the end
    * of the lines
    *
    */
  val lines: Iterable[String] = l.map(_.trim)

  /** Converts lines to a newline-delimited string of lines each ending with a
    * break element
    *
    * @return
    */
  private def htmlifiedLines: String = lines.map(_ + "<br />").mkString("\n")

  /** Converts the cleaned lines to an HTML string
    *
    * @return HTML stringified output lines
    */
  override def toString: String =
    s"""<html>
      |<meta http-equiv='Content-Type' content='text/html; charset=UTF-8' />
      |<body>
      |$htmlifiedLines
      |</body>
      |</html>""".stripMargin

  /** Writes the HTML stringified lines to a file
    *
    * @param filename the name of the output file
    */
  def toHtmlFile(filename: String): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(this.toString)
    bw.close()
  }
}
