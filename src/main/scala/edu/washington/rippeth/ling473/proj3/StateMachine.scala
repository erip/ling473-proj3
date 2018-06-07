package edu.washington.rippeth.ling473.proj3

import com.typesafe.scalalogging.LazyLogging

/** Given input lines, moves through the state machine for
  * each line
  *
  * @param lines the unsegmented Thai lines
  */
class StateMachine(lines: Iterable[String]) extends LazyLogging {

  // The categories that force transitions
  private final val V1: Set[Char] = "เแโใไ".toSet
  private final val C1: Set[Char] = "กขฃคฅฆงจฉชซฌญฎฏฐฑฒณดตถทธนบปผฝพฟภมยรฤลฦวศษสหฬอฮ".toSet
  private final val C2: Set[Char] = "รลวนม".toSet
  private final val V2: Set[Char] = "◌ิ◌ี◌ึ◌ื◌ุ◌ู◌ั◌็".toSet
  private final val T: Set[Char] = Set('\u0E48', '\u0E49', '\u0E4A', '\u0E4B')
  private final val V3: Set[Char] = "าอยว".toSet
  private final val C3: Set[Char] = "งนมดบกยว".toSet

  // The following methods define the actions to be performed
  // (i.e., defining to which state the machine should transition)
  // The naming convention is actionN where N is the state before
  // transition

  private def action0(implicit oldState: State, c: Char): State =
    if(V1.contains(c)) {
      logger.trace("Going to state 1")
      State1(oldState.stringBuilderOut, Some(c))
    } else if(C1.contains(c)) {
      logger.trace("Going to state 2")
      State2(oldState.stringBuilderOut, Some(c))
    } else
      throw new IllegalArgumentException(s"Invalid input '$c'")

  private def action1(oldState: State, c: Char): State =
    if(C1.contains(c)) {
      logger.trace("Going to state 2")
      State2(oldState.stringBuilderOut, Some(c))
    } else
      throw new IllegalArgumentException(s"Invalid input '$c'")

  private def action2(oldState: State, c: Char): State =
    if(C2.contains(c)) {
      logger.trace("Going to state 3")
      State3(oldState.stringBuilderOut, Some(c))
    } else if(V2.contains(c)) {
      logger.trace("Going to state 4")
      State4(oldState.stringBuilderOut, Some(c))
    } else if(T.contains(c)) {
      logger.trace("Going to state 5")
      State5(oldState.stringBuilderOut, Some(c))
    } else if(V3.contains(c)) {
      logger.trace("Going to state 6")
      State6(oldState.stringBuilderOut, Some(c))
    } else if(C3.contains(c)) {
      logger.trace("Going to state 9")
      State9(oldState.stringBuilderOut, Some(c))
    } else if(V1.contains(c)) {
      logger.trace("Going to state 7")
      State7(oldState.stringBuilderOut, Some(c))
    } else if(C1.contains(c)) {
      logger.trace("Going to state 8")
      State8(oldState.stringBuilderOut, Some(c))
    } else
      throw new IllegalArgumentException(s"Invalid input '$c'")

  private def action3(oldState: State, c: Char): State =
    if(V2.contains(c)) {
      logger.trace("Going to state 4")
      State4(oldState.stringBuilderOut, Some(c))
    } else if(T.contains(c)) {
      logger.trace("Going to state 5")
      State5(oldState.stringBuilderOut, Some(c))
    } else if(V3.contains(c)) {
      logger.trace("Going to state 6")
      State6(oldState.stringBuilderOut, Some(c))
    } else if(C3.contains(c)) {
      logger.trace("Going to state 9")
      State9(oldState.stringBuilderOut, Some(c))
    } else
      throw new IllegalArgumentException(s"Invalid input '$c'")

  private def action4(oldState: State, c: Char): State =
    if(T.contains(c)) {
      logger.trace("Going to state 5")
      State5(oldState.stringBuilderOut, Some(c))
    } else if(V3.contains(c)) {
      logger.trace("Going to state 6")
      State6(oldState.stringBuilderOut, Some(c))
    } else if(C3.contains(c)) {
      logger.trace("Going to state 9")
      State9(oldState.stringBuilderOut, Some(c))
    } else if(V1.contains(c)) {
      logger.trace("Going to state 7")
      State7(oldState.stringBuilderOut, Some(c))
    } else if(C1.contains(c)) {
      logger.trace("Going to state 8")
      State8(oldState.stringBuilderOut, Some(c))
    }
    else
      throw new IllegalArgumentException(s"Invalid input '$c'")

  private def action5(oldState: State, c: Char): State =
    if(V3.contains(c)) {
      logger.trace("Going to state 6")
      State6(oldState.stringBuilderOut, Some(c))
    } else if(C3.contains(c)) {
      logger.trace("Going to state 9")
      State9(oldState.stringBuilderOut, Some(c))
    } else if(V1.contains(c)) {
      logger.trace("Going to state 7")
      State7(oldState.stringBuilderOut, Some(c))
    } else if(C1.contains(c)) {
      logger.trace("Going to state 8")
      State8(oldState.stringBuilderOut, Some(c))
    } else
      throw new IllegalArgumentException(s"Invalid input '$c'")

  private def action6(oldState: State, c: Char): State =
    if(C3.contains(c)) {
      logger.trace("Going to state 9")
      State9(oldState.stringBuilderOut, Some(c))
    } else if(V1.contains(c)) {
      logger.trace("Going to state 7")
      State7(oldState.stringBuilderOut, Some(c))
    } else if(C1.contains(c)) {
      logger.trace("Going to state 8")
      State8(oldState.stringBuilderOut, Some(c))
    } else
      throw new IllegalArgumentException(s"Invalid input '$c'")

  // Because actions 7, 8, and 9 don't consume input,
  // they will simply be a pass-through to the action of the
  // state for which they are acting as a proxy
  private def action7(oldState: State, c: Char): State = action1(oldState, c)
  private def action8(oldState: State, c: Char): State = action2(oldState, c)
  private def action9(oldState: State, c: Char): State = action0(oldState, c)

  /** Given a state and a character, choose the next action to perform
    * (i.e., choose the next state)
    *
    * @param state the state of the machine right now
    * @param c the character triggering a transition
    * @return the new state of the machine
    */
  private def segmentationFunction(state: State, c: Char): State = state match {
    case s: State0 => action0(s, c)
    case s: State1 => action1(s, c)
    case s: State2 => action2(s, c)
    case s: State3 => action3(s, c)
    case s: State4 => action4(s, c)
    case s: State5 => action5(s, c)
    case s: State6 => action6(s, c)
    case s: State7 => action7(s, c)
    case s: State8 => action8(s, c)
    case s: State9 => action9(s, c)
  }

  logger.trace("In state 0")
  private final def initialState: State = State0(new StringBuilder, None)

  /** For a given line, this function will work through
    *  the entire transition of state in the state machine.
    *
    * @param line the line being processed
    * @return the final state
    */
  private def transition(line: String, initialState: State): State = {
    // Given a state (with initial value of State0(new StringBuilder)) and a character
    // (left to right in the string), apply segmentationFunction. Update
    // the state and move to next character in the line.
    val state = line.foldLeft(initialState) { (state, c) =>
      logger.trace(s"Handling $c")
      segmentationFunction(state, c)
    }
    logger.debug(s"Processed line: ${state.stringBuilderOut.toString()}")
    state
  }

  /** Apply transition to the given line and return the output string from
    * the resultant state
    *
    * @param line the line being processed
    * @return the output string of the final state
    */
  private def segmentLine(line: String): String = transition(line, initialState).stringBuilderOut.toString

  /** Segments all lines
    *
    * @return the SegmentedLines of all the inputs
    */
  def segmentLines: SegmentedLines = new SegmentedLines(lines.map(segmentLine))
}