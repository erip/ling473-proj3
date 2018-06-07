package edu.washington.rippeth.ling473

package object proj3 {

  private[proj3] def optionallyAppend(sb: StringBuilder, char: Option[Char]): StringBuilder = char match {
    case Some(c) =>
      sb.append(c)
      sb
    case None =>
      sb
  }

  /** Inserts a space before the last element.
    *
    * @param sb the string to which a space should be added
    * @return the string with a space at the n-1st position
    */
  private[proj3] def addSpaceBeforeLast(sb: StringBuilder): StringBuilder = {
    sb.insert(sb.length-1, ' ')
    sb
  }

  /** Appends a space to the StringBuilder.
    *
    * @param sb the StringBuilder to which a space should be added
    * @return the StringBuilder with a space at the end
    */
  private[proj3] def addSpaceAtEnd(sb: StringBuilder): StringBuilder = {
    sb.append(' ')
    sb
  }
}
