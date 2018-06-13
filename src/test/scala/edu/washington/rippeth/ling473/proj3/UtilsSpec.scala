package edu.washington.rippeth.ling473.proj3

import org.scalatest.{Matchers, WordSpecLike}

class UtilsSpec extends WordSpecLike with Matchers {

  private def withCleanStringBuilder(block: StringBuilder => Any): Unit = {
    val sb = new StringBuilder
    block(sb)
  }

  "optionallyAppend" should {
    "append when the character is not None" in withCleanStringBuilder { sb =>
      sb shouldBe empty
      val charToAdd = 'c'
      val char @ Some(c) = Some(charToAdd)
      val newSb = optionallyAppend(sb, char)
      newSb.length should===(1)
      newSb.headOption shouldBe char
    }

    "not append when the character is None" in withCleanStringBuilder { sb =>
      sb shouldBe empty
      val char = None
      val newSb = optionallyAppend(sb, char)
      newSb shouldBe empty
    }
  }

  "addSpaceBeforeLast" should {
    "add a space before the last character" in withCleanStringBuilder { sb =>
      val (first, last) = ("hell", "o")
      val totalLength = first.length + last.length
      sb.appendAll(s"$first$last")
      val newSb = addSpaceBeforeLast(sb)
      // Added a space, so length is updated
      newSb.length should===(totalLength + 1)
      val expectedString = s"$first $last"
      newSb.toString should===(expectedString)
    }
  }

  "addSpaceAtEnd" should {
    "add a space at the last character" in withCleanStringBuilder { sb =>
      val expectedString = "hello "
      sb.appendAll(expectedString.trim)
      val newSb = addSpaceAtEnd(sb)
      newSb.toString should===(expectedString)
    }
  }

}
