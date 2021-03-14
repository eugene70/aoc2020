package aoc2020

object Template:
  type InputType = List[String]

  @main def day: Unit =
    val testData = time("testReady", () => ready(testInput))
    time("tesPart1", () => part1(testData))
    time("testPart2", () => part2(testData))

    val data = time("ready", () => ready(input))
    time("part1", () => part1(data))
    time("part2", () => part2(data))

  private def ready(input: String): InputType =
    input
      .linesIterator
      .toList

  private def part1(data: InputType): Long =
    0

  private def part2(data: InputType): Long =
    0

  private val testInput =
    """
      |""".stripMargin

  private val input =
    """
      |""".stripMargin
