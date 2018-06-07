package edu.washington.rippeth.ling473.proj3


/** A base class of any state that the machine could take
  *
  * @param stringBuilderIn the input string builder at the current tape
  * @param char the optional char to append to the string builder
  */
private[proj3] sealed abstract class State(stringBuilderIn: StringBuilder, char: Option[Char] = None) {
  val stringBuilderOut: StringBuilder = optionallyAppend(stringBuilderIn, char)
}

// State0 through State6 are very vanilla -- just appending
// a character to the output string
private[proj3] final case class State0(sb: StringBuilder, c: Option[Char]) extends State(sb, c)
private[proj3] final case class State1(sb: StringBuilder, c: Option[Char]) extends State(sb, c)
private[proj3] final case class State2(sb: StringBuilder, c: Option[Char]) extends State(sb, c)
private[proj3] final case class State3(sb: StringBuilder, c: Option[Char]) extends State(sb, c)
private[proj3] final case class State4(sb: StringBuilder, c: Option[Char]) extends State(sb, c)
private[proj3] final case class State5(sb: StringBuilder, c: Option[Char]) extends State(sb, c)
private[proj3] final case class State6(sb: StringBuilder, c: Option[Char]) extends State(sb, c)

// State7 and State8 are special -- add a space before the last character
// in the output string
private[proj3] final case class State7(sb: StringBuilder, c: Option[Char])
  extends State(addSpaceBeforeLast(optionallyAppend(sb, c)))
private[proj3] final case class State8(sb: StringBuilder, c: Option[Char])
  extends State(addSpaceBeforeLast(optionallyAppend(sb, c)))

// State9 is also special -- add a space to the end of the output string
private[proj3] final case class State9(sb: StringBuilder, c: Option[Char])
  extends State(addSpaceAtEnd(optionallyAppend(sb, c)))