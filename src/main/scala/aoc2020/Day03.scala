package aoc2020

object Day03:
  type InputType = Array[Array[Char]]

  private def ready(input: String): InputType =
    input.linesIterator
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(_.toCharArray)
      .toArray
  
  @main def solveDay03: Unit =
    var testData: InputType = time("testReady", () => ready(testInput))
    time("testSolve1", () => solve1(3, 1)(testData)) // should be 7
    time("testSolve2", () => solve2(testData)) // should be 336

    var data: InputType = time("ready", () => ready(input))
    time("solve1", () => solve1(3, 1)(data))
    time("solve2", () => solve2(data))

  private def solve1(dx: Int, dy: Int)(implicit data: InputType): Long =
    solve11(0, 0)(dx, dy)

  private def solve11(x: Int, y: Int)(dx: Int, dy: Int)(implicit slope: InputType): Int = {
    if (y >= slope.length) return 0
    val xx = x % slope(y).length
    (if (slope(y)(xx) == '#') 1 else 0) + solve11(xx + dx, y + dy)(dx, dy)
  }

  private def solve2(implicit data: InputType): Long =
    val steps: Array[(Int, Int)] = Array(
      (1, 1), (3, 1), (5, 1), (7, 1), (1, 2)
    )
    steps.map(st => solve1(st._1, st._2).toLong).product

  private val testInput =
    """..##.......
      |#...#...#..
      |.#....#..#.
      |..#.#...#.#
      |.#...##..#.
      |..#.##.....
      |.#.#.#....#
      |.#........#
      |#.##...#...
      |#...##....#
      |.#..#...#.#
      |""".stripMargin

  private val input =
    """....##..#........##...#.#..#.##
      |.#.#..#....##....#...#..##.....
      |##.#..##..#...#..........##.#..
      |.#.##.####..#......###.........
      |#.#.#...........#.....#...#....
      |#.......#....#.#.##..###..##..#
      |.#...#...##....#.........#.....
      |..........##.#.#.....#....#.#..
      |.......##..##...#.#.#...#......
      |.#.#.#...#...##...#.##.##..#...
      |........##.#.#.###.........##..
      |#.#..#.#.#.....#...#...#......#
      |.#.#.#...##......#...#.........
      |.#..##.##.#...#...##....#.#....
      |.##...#..#..#......##.###....##
      |.....#...#.###.....#.#.........
      |#.##..#....#.#.#.#.............
      |........#...#......#...#..#....
      |##..##...##.##...#...#.###...##
      |#.#....##.#...###......#..#.#..
      |..#.....#.##......#..........#.
      |#.......#..#......#.....#....#.
      |.....###...........#....#.##...
      |#.#........##.......#.#...#.##.
      |.#.#.#........#........#.#.....
      |#..#..##.....#.###..#.#.#.##..#
      |..#.#...#..##.#.#.#.......###..
      |........#........#..#..#...#...
      |##............#...#..##.##...#.
      |#....#.#.....##...#............
      |............#...#..#.#.#....#..
      |#.#.#...##.##.#....#....#......
      |................###.....#.....#
      |##.#####.#..#...###..#...###...
      |...#.....#...#.#....#...#..#...
      |.......#....##.##.#.##.........
      |..#..#..##.....#...#.#.....#...
      |...#...#.#.##.#..###.......#...
      |...#...........#.#####..##..#..
      |#.#...#........####..#......#.#
      |#..#.##...........#.#......#.##
      |#.#..#....##..#..##.#..........
      |.....#..#.....#........##..#...
      |...###.......#.##.......#......
      |...##..#..#...#....#.###...#...
      |....####....#........#.##.#.#.#
      |#....#.....#.###.##...##..##.##
      |.##.#...#.##.#......#..##.#....
      |...#.............#.............
      |..##..##.#.....#........##....#
      |#.....#....#.......####...#..#.
      |..#...#..#...#...##.#....##....
      |.#...##....#....#..#....#......
      |##..#.#...##......#..#.......##
      |..#...#.##..#.....#.#...#..#.#.
      |#..##....#..........#..........
      |.#........#..#......#......#.#.
      |...##.#.........#.#....#.#...#.
      |#.....#.#..#...#...#..#...#...#
      |#.........#.#.........##.......
      |.#.......#......#.........###..
      |.#..#..##...........#.....#..#.
      |.#....................#.....#..
      |.##.....#....#....#.###.....#..
      |...##.#.....#.#....#.........#.
      |.........##.....#.....#.##..#..
      |......#......#.#..#..###...#..#
      |..##...#.#..#...#.#....#.......
      |..#..##.###.#..#..#..#......#..
      |.....##...##.........#...##...#
      |###.#..##....##...##.#..###....
      |#...#.#..##......##...#.#..#...
      |..........#....###....#........
      |#.#.#.#.#.....#..##.##.....#...
      |.##.....#...#.....#......#.....
      |.#..........#.#.............#..
      |.#..##..#.#..##...#....#.##...#
      |..#.#..........#...#..........#
      |.#.......#.......#...#..#.....#
      |##.#...##...#......#.#..#......
      |#####..#....#..#...#...#.#.....
      |....#.......#.#..#.............
      |#..#..#.#.####...#....#....##..
      |#..#.##.#......#...#......#....
      |#...##.##...#....#..........##.
      |..#..#.......##.#....#...#.#...
      |.....#.##..............##.....#
      |..##.##...##.....#.........#.#.
      |.#....##...##...##..#....##..#.
      |.#...#....#..#......#.#........
      |#....#.#.#..............#....##
      |..##..#..#....#####.#....#.#.##
      |#....#...#.##..#.##.........###
      |#..#..#....#...............#.#.
      |#....##...##........##.##.#.##.
      |......#......##....##.....#.###
      |#...##..#..#.....#.#........##.
      |..#.#.##...#...#....#..###...#.
      |#..##..#.###..##.#.#....#......
      |..###..#.##........###.....#...
      |#.............#.............#..
      |.#.##....#..##.#...#....#.#####
      |###.....###.#......##..#..##...
      |.#.#......##.#....#....#.#..#..
      |###..#..#....#......###.##.....
      |......#.......#......#..##..##.
      |..#..#...#..#....#.##....#.#..#
      |.......##..#........#.#.##.....
      |.#...#..#........#..#.#..#..#.#
      |.#..#.##.......#......#...#.#..
      |.##..##......##.#...#......####
      |.....#....#......#.....#......#
      |..........#.#.#...##.#..#.#....
      |...#.......##......#..#.#.##...
      |.###..#.#.#....................
      |##...#...#.##............#.....
      |....#....#.##...#..#.#...##....
      |..#.#....#.###...#...#.#.####.#
      |..#..#.#...#.#......##.........
      |#..#..####.##.#.#..###....#...#
      |....#..........#.##.#..#.#.#.#.
      |#.#.##.........#.....##...#..##
      |#......#...#.##.#......#..#.#..
      |#...#........#..#..#...##...#..
      |.....#.####..##..#.#.##..#...#.
      |#..#........#........#...#....#
      |...........#..#.....#.#.#.#....
      |....#......#....#...#....##....
      |.#.#..#...#.#....#..#.#....##.#
      |....#...#...#.##..#...#..##...#
      |#######...............##.....##
      |.#.#..............#....#..#.###
      |#......#.#......###....###.....
      |##..#...#.##..##..##.#....#....
      |#....##..#..#...#.#.#...#......
      |..........#..#.##..##.##.#..##.
      |....#.#.#.....##........#..#...
      |..###...#.....##.##.....##..##.
      |....#.#..#.#.......#.......#...
      |..##.#..#.....##...###...#...#.
      |..#.........#...##...#...#..#..
      |..#..#..#..#..##.#...##..#.#...
      |...##..#..##..#..####...#.....#
      |............#............###...
      |.#.#.###.#.....#.#.#..#.###..#.
      |...#.........#.....####........
      |....##.#..##.#.............#...
      |....#.##.....#..#.....#......##
      |..........#.............#...##.
      |#..#.....#.......##..###.......
      |..##.#...........#.......#..#..
      |...#...#.#.##.###....#.#..#....
      |...#..........##..#..#..#...###
      |.........#.....#..##.....#..#..
      |#........#...#...#..#.#....##..
      |.#.#.....####..#.##.#..........
      |###.......##..#.##...#.....#...
      |..###.##.#..#..#..#.....##...#.
      |.........#.....##.#..#..##.....
      |#..#..##...###..............#..
      |#....#.#....#..#.....#..####...
      |####..#.....##...#..#.#.#.#...#
      |...#....#.....#.##.#.#.#....##.
      |..........#...#.....###....#.##
      |#....#.#.#....#..#..#.....#....
      |.....#.#...#......#....#......#
      |.####....##...#...#......##..#.
      |.#...#..#....#..#..............
      |##.#...##...#.##..#......#.....
      |..####.##..#....#.#......#.#.##
      |........#.....##...#.#..##....#
      |....#.#.#.#.###...#.#...##...##
      |#.....#...####.#....#.#........
      |..#.....#...##.........###.....
      |....#....#....#..#......#####.#
      |###.....#..#.#.#......#.##.#...
      |....#.##......#..#.#...........
      |.#....#....#.#..#.......#...##.
      |...................#.#.#..#....
      |##...#.....#........#....#...#.
      |........##......#...##.#..#.#.#
      |#.#..###...#....#.#...#.......#
      |#..........##......#..#..#.....
      |.............#...##.#...#......
      |#..#....##..#.........#..#.###.
      |.....#..........#....##.#...##.
      |###....................#.#.##..
      |#........##...##......#....###.
      |#....#.............#....#...#..
      |##.......##.#.......#....#..#..
      |##...#............#..#.#....##.
      |..#.#..#...#####..........#....
      |..#.........##.....#.#...####..
      |....#..............#...........
      |..#...#.#.#..#.......##.#.#.#..
      |....#.##.....##..#.....#..####.
      |#...#...#...#.......#.........#
      |......#..#.####...###.#.#.....#
      |.......#..#..#.....#.#..##.#..#
      |.#......##..#............#.....
      |.#........#.#....#....#........
      |.....#.#..#.##.#..##....#..#...
      |#.#...........#....##.....#....
      |..#..#..##.###..##..#..###.#.##
      |##.#..#...##.#.........#...#.#.
      |......#..#..##...#....#...#.##.
      |#.##.......................#...
      |.......#..#..#.##..##......#...
      |..#.#...#.....#..###....#...#..
      |##..#.....#..#..#.##.....#...##
      |#...##...###...#.#..###....#...
      |...#.#.#..####.....#...##....#.
      |.#.#..##.....#..#.....##..##..#
      |#...#..........#.##.#.#........
      |..##....#.##....#..##......#...
      |....#..........###.....####..##
      |...........###....##.#.#.#.#...
      |...#......................####.
      |#.#.#...#.#.#.#.#......#.....##
      |..###...#.####...#..##..#....#.
      |....#....#.......#...#.........
      |.#.###.............##..#...#...
      |....#.#....##...#.....#.##.....
      |#.......##.....#.#.....#....##.
      |....##.....###..#.#..#....#...#
      |......#..##...#......#.....#.##
      |.#.....#.##.###....#.....#..###
      |...#..#.###.#....#..#..#...##.#
      |...##..#...#..#.#.#..#...#.....
      |##.####...##..#.#.#....#.......
      |..##..#.#.......##.#......##.#.
      |....##....#....#..#....#..##.#.
      |..##..........##....#...#.#..#.
      |#.#...#.#.###.#.#..##.#...#....
      |.....#..#.............#...#...#
      |....#.#..#...##...#....#.##....
      |#..#...#.###.....#...#.....#.#.
      |#####....#....#....#.......#.##
      |#...##....##.#.#...#.....##.#..
      |#.......#...#..#..#...#....#...
      |....#......#.#..........#....##
      |#.###...#.#..##..#.##........#.
      |#..#.....##.......#..#..#.#....
      |...#...#.##...#....#.#.#.#...#.
      |...##..#.#....#......###......#
      |#.#....#...#..#..#....#........
      |..#..#.#...##..........#.###...
      |#..........#...#..#....#....###
      |..#..#.#....#..............#...
      |...#........#...#.#....###.#..#
      |....#.#.#................#..#.#
      |..#........##.#....#.#..#......
      |...##..#..#.......#..#......#.#
      |..#..#....#.........#....#.##..
      |#.....#....###.#..#..#...#...#.
      |..#..##.###.#..##....#.###.....
      |...#...####..#........###.#....
      |.........#.#...#..#..#.#.......
      |.##.........##.#..............#
      |..#.#.#.....###........#.#.#..#
      |....##..#....#....#.#..#.......
      |#.#.....#...#........##........
      |.##.#.#..#..#.#.#.........#....
      |#.....#..#.##...#...#..........
      |##..#....#....##.#..#.........#
      |................#.##.#......#.#
      |..#..#.#........##...###..#...#
      |##........#.......#...##.##..#.
      |##....#.....#..##....#.......#.
      |#.#....#.#........#..#.........
      |......##......#...#.....#.##...
      |###....#..........##.#.#......#
      |......#...###.........###..#...
      |.####....#...##..#.#.....#...#.
      |.##...#...###....#...#.#..###..
      |#..#......##...#.###..###...#..
      |#....#.#.#..#....##...#.##..#..
      |..#.....#...#..........#.##.###
      |#.....#....###.......##..##.#..
      |#..##...#..#....#.###......#...
      |#..#........##..#.....#.#.#....
      |#.##.#.#..#....#.#.............
      |.#...............#....##.......
      |.#.##......##........#...#..#.#
      |.#...#....#....#...#..#...##...
      |.....#..###...##........#.#....
      |...#.......#....##..#..#....#..
      |...###....#........#..#.###.#..
      |......##..##..............###.#
      |.......#.####..##....#.#....#..
      |#...#......#...#..#.....##....#
      |.#..#..###....#..##.##.#.......
      |#......##......#..##....#..##..
      |.....#..#.#......##.##..##.....
      |...#..#.......#......#.........
      |....#..####......#..#....#...#.
      |..#.#..#...#....#....#.......#.
      |####..#........#.###...##.#.#.#
      |.......##........#.#.#...##....
      |...#.........#..#.#..##....#...
      |.....#..#...#.#....#...#.#.##.#
      |#..##.....#.....##.......#...#.
      |.......##.#.#.....#....#......#
      |...#...#.##...#......#....#....
      |..#..#.#...#..#.....#...###.#..
      |.........#...#..#.......##.....
      |..##...................#..#.###
      |.##.##..#.#...#.#....#.....##..
      |#.#...##...#...#...##..#......#
      |....#..#...#.....##.#.....#..##
      |##.#..........###..#...#..#....
      |...##....#.##....#......#......
      |.....#.........#....#.#.......#
      |.......#............#.#.....#..
      |..#..#...#..#####..#....##.....
      |...##......##...#.#........##..
      |.....#..###...##.#.#.##.#...#..
      |..#.#.#..##..#.##...##.#.#.....
      |......##...#..##......#.#......
      |......................#........
      |#...#..#....#..#.#.##.#.....#.#
      |.#......#.#....#.#.#..#....#...
      |.#..#.#.#..#....#..............
      |""".stripMargin
