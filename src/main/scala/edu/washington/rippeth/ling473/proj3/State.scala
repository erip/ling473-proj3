package edu.washington.rippeth.ling473.proj3

private[proj3] sealed trait State {
  /**
    * The string builder before a character is potentially applied.
    */
  protected def stringBuilderIn: StringBuilder

  /**
    * The character which can trigger an update in state.
    *
    * Creating an Option[Char] is a hack because the empty char literal does not
    * exist in Scala.
    */
  protected def char: Option[Char]

  /**
    * An optional function which can transform the tape after the string builder
    * may have been updated. This will be `None` in the normal case.
    */
  protected def postProcessesStringBuilder: Option[StringBuilder => StringBuilder]

  /**
    * If the state is a "normal" state
    * @return
    */
  def stringBuilderOut: StringBuilder = postProcessesStringBuilder match {
      case Some(f) => f(optionallyAppend(stringBuilderIn, char))
      case None => optionallyAppend(stringBuilderIn, char)
    }
}

private[proj3] sealed trait NormalState extends State {
  /**
    * In the normal case, we don't post process the string builder -- potentially adding
    * the character is good enough.
    * @return
    */
  final protected def postProcessesStringBuilder: Option[StringBuilder => StringBuilder] = None
}

private[proj3] sealed trait StateWithPenultimateSpace extends State {
  /**
    * In state 7 and 8, a space is to be added before the penultimate character,
    * so this will be the post-processing function.
    */
  final protected def postProcessesStringBuilder: Option[StringBuilder => StringBuilder] =
    Some(addSpaceBeforeLast)
}

private[proj3] sealed trait StateWithFinalSpace extends State {
  /**
    * In state 9, a space is to be appended to the string, so this will be
    * the post-processing function.
    */
  final protected def postProcessesStringBuilder: Option[StringBuilder => StringBuilder] =
    Some(addSpaceAtEnd)
}

// State0 through State6 are very vanilla -- just appending
// a character to the output string
private[proj3] final case class State0(stringBuilderIn: StringBuilder, char: Option[Char])
  extends NormalState
private[proj3] final case class State1(stringBuilderIn: StringBuilder, char: Option[Char])
  extends NormalState
private[proj3] final case class State2(stringBuilderIn: StringBuilder, char: Option[Char])
  extends NormalState
private[proj3] final case class State3(stringBuilderIn: StringBuilder, char: Option[Char])
  extends NormalState
private[proj3] final case class State4(stringBuilderIn: StringBuilder, char: Option[Char])
  extends NormalState
private[proj3] final case class State5(stringBuilderIn: StringBuilder, char: Option[Char])
  extends NormalState
private[proj3] final case class State6(stringBuilderIn: StringBuilder, char: Option[Char])
  extends NormalState

// State7 and State8 are special -- add a space before the last character
// in the output string
private[proj3] final case class State7(stringBuilderIn: StringBuilder, char: Option[Char])
  extends StateWithPenultimateSpace
private[proj3] final case class State8(stringBuilderIn: StringBuilder, char: Option[Char])
  extends StateWithPenultimateSpace

// State9 is also special -- add a space to the end of the output string
private[proj3] final case class State9(stringBuilderIn: StringBuilder, char: Option[Char])
  extends StateWithFinalSpace