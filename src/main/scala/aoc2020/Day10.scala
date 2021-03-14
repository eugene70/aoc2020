package aoc2020

object Day10:
  type InputType = List[Int]
   
  @main def _day10: Unit =
    val testData = time("testReady", () => ready(testInput))
    time("tesPart1", () => part1(testData))
    time("testPart2", () => part2(testData))
    time("testPart2_2", () => part2_2(testData))

    val data = time("ready", () => ready(input))
    time("part1", () => part1(data))
    time("part2", () => part2(data))
    time("part2_2", () => part2_2(data))

  private def ready(input: String): InputType =
    val in = input
      .linesIterator
      .map(_.toInt)
      .toList
      .sorted
    0 +: in :+ (in.last + 3)

  private def part1(data: InputType): Long =
    val part = data
      .sliding(2)
      .map(t => t.last - t.head)
      .filter(a => a == 1 || a == 3)
      .partition(_ == 1)
    part._1.length * part._2.length

  private def part2(implicit data: InputType): Long =
    var i = 0
    var j = 1
    var result: Long = 1
    while
      if (data(j) - data(i) == 1) {
        while (j < data.length && i < j)
          if (data(j + 1) - data(j) == 1) {
            j += 1
          } else {
            result = result * repeat(data(i), 0)(data.slice(i, j + 1))
            i = j
          }
      } else {
        i += 1
      }
      j = i + 1
      i < data.length - 1
    do()
    result

  private def repeat(v: Int, n: Int)(implicit data: InputType): Long =
    if (n >= data.length) return 0
    val value = data(n)
    if (value - v > 3) return 0
    if (n == data.length - 1) return 1
    repeat(value, n + 1) + repeat(value, n + 2) + repeat(value, n + 3)

  private def part2_2(data: InputType): Long =
    data
      .sliding(2)
      .map(a => (a.last - a.head) % 3)
      .sliding(3)
      .map(a => {println(a); a})
      .map {
        case Seq(1, 1) => 2L
        case _ => 1L
      }
      .reduce(_ * _)

  private val testInput =
    """16
      |10
      |15
      |5
      |1
      |11
      |7
      |19
      |6
      |12
      |4
      |""".stripMargin

  private val input =
    """151
      |94
      |14
      |118
      |25
      |143
      |33
      |23
      |80
      |95
      |87
      |44
      |150
      |39
      |148
      |51
      |138
      |121
      |70
      |69
      |90
      |155
      |144
      |40
      |77
      |8
      |97
      |45
      |152
      |58
      |65
      |63
      |128
      |101
      |31
      |112
      |140
      |86
      |30
      |55
      |104
      |135
      |115
      |16
      |26
      |60
      |96
      |85
      |84
      |48
      |4
      |131
      |54
      |52
      |139
      |76
      |91
      |46
      |15
      |17
      |37
      |156
      |134
      |98
      |83
      |111
      |72
      |34
      |7
      |108
      |149
      |116
      |32
      |110
      |47
      |157
      |75
      |13
      |10
      |145
      |1
      |127
      |41
      |53
      |2
      |3
      |117
      |71
      |109
      |105
      |64
      |27
      |38
      |59
      |24
      |20
      |124
      |9
      |66
      |""".stripMargin
