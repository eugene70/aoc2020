package aoc2020

object Template:
  type InputType = List[String]

  @main def runTemplate: Unit =
    val testData = time("testReady", () => ready(testInput))
    time("testPart1", () => part1(testData))
    time("testPart2", () => part2(testData))

    val data = time("ready", () => ready(input))
    time("part1", () => part1(data))
    time("part2", () => part2(data))

  def ready(input: String): InputType =
    input
      .linesIterator
      .toList

  def part1(data: InputType): Long =
    0

  def part2(data: InputType): Long =
    0

  val testInput =
    """
      |""".stripMargin

  val input =
    """
      |""".stripMargin
