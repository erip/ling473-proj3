package edu.washington.rippeth.ling473.proj3

import org.scalatest.FlatSpec

class StateMachineTest extends FlatSpec {

  "A StateMachine" should "segment the example" in {

    val input: Seq[String] = Seq("แบ่งแผ่นดินออกเป็นสองสว่น", "หลุมพอต้นใหญ่งอกงามชิดตลิ่ง")
    val expectedOutput = Seq("แบ่ง แผ่น ดิน ออก เป็น สอง สว่น", "หลุม พอ ต้น ให ญ่ง อก งาม ชิด ตลิ่ง")
    val stateMachine: StateMachine = new StateMachine(input)

    assertResult(expectedOutput) {
      stateMachine.segmentLines.lines
    }
  }

}
