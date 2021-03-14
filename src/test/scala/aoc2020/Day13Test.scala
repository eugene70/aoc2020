package aoc2020

import org.junit.Assert.assertEquals
import org.junit.Test

class Day13Test:
  import Day13._
  
  @Test def testGcd(): Unit =
    assertEquals(1, Day13.gcd(12345, 1234))
    assertEquals(3, Day13.gcd(9, 6))

  @Test def TestLcm(): Unit =
    assertEquals(15233730, Day13.lcm(12345, 1234))
    assertEquals(18, Day13.lcm(9, 6))
  
  @Test def testPart2: Unit =
    val data = (1001938, List("5", "7"))
    time("part2 test", () => part2(data))
    println("[3, 5, 7] = " + lcm(lcm(3, 5), 7))
    println(lcm(5, 7))
    println(lcm(6, 7))
    println(lcm(5, 8))
