package aoc2020

import scala.annotation.tailrec

object Day25:
  type InputType = List[Long]

  @main def runDay25: Unit =
    val testData = time("testReady", () => ready(testInput))
    time("testPart1", () => part1(testData)) // should be 14897079
    //time("testPart2", () => part2(testData))

    val data = time("ready", () => ready(input))
    time("part1", () => part1(data))
    //time("part2", () => part2(data))

  def ready(input: String): InputType =
    input
      .linesIterator
      .map(_.toLong)
      .toList

  @tailrec
  def findLoop(count: Int, subjectNumber: Int, publicKey: Long, value: Long): Int =
    if (value == publicKey) return count;
    val newValue: Long = value * subjectNumber % 20201227
    findLoop(count + 1, subjectNumber, publicKey, newValue)

  @tailrec
  def loop(count: Int, subjectNumber: Long, value: Long): Long =
    if (count == 0) return value;
    loop(count - 1, subjectNumber, value * subjectNumber % 20201227)

  def part1(data: InputType): Long =
    val loop1 = findLoop(0, 7, data(0), 1L)
    loop(loop1, data(1), 1L)

  def part2(data: InputType): Long =
    0

  val testInput =
    """5764801
      |17807724""".stripMargin

  val input =
    """9717666
      |20089533""".stripMargin
