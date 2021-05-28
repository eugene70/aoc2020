package aoc2020

object Day17:
  type InputType = Array[Array[Int]]
  type ThreeD = Array[Array[Array[Int]]]

  @main def runDay17: Unit =
    val testData = time("testReady", () => ready(testInput))
    time("tesPart1", () => part1(testData)) // should be 112
//    time("testPart2", () => part2(testData))

    val data = time("ready", () => ready(input))
//    time("part1", () => part1(data))
//    time("part2", () => part2(data))

  private def ready(input: String): InputType =
    input
      .linesIterator
      .map(_.map(c => if (c == '#') 1 else 0).toArray).toArray

  private def part1(data: InputType): Long = {
    val arr3d = trans(make3d(data))
    for (i <- 0 to arr3d.size - 1) {
      for (j <- 0 to arr3d(i).size - 1) {
        for (k <- 0 to arr3d(i)(j).size - 1)
          print(arr3d(i)(j)(k))
        println
      }
      println
    }
    0
  }

  def make3d(data: InputType): ThreeD =
    val arr3d = Array.ofDim[Int](data.size, data.size, data.size)
    for (i <- 0 to data.size - 1) {
      for (j <- 0 to data.size - 1)
        for (k <- 0 to data.size - 1)
          arr3d(i)(j)(k) = data(j)(k)
    }
    arr3d

  def trans(arr: ThreeD): ThreeD = {
    val arrTo = Array.ofDim[Int](arr.size, arr.size, arr.size)
    for (i <- 0 to arr.size - 1)
      for (j <- 0 to arr.size - 1)
        for (k <- 0 to arr.size - 1)
          arrTo(i)(j)(k) = transOne(arr)(i, j, k)
    arrTo
  }

  def transOne(dim1: ThreeD)(x: Int, y: Int, z: Int): Int = {
    val size = dim1.size
    val p = dim1(x)(y)(z)
    var neighbors = Seq.empty[Int]
    for (i <- x - 1 to x + 1)
      for (j <- y - 1 to y + 1)
        for (k <- z - 1 to z + 1) {
            if (i != x || j != y || k != z)
              neighbors = neighbors :+ dim1(i)(j)(k)
        }
    println(neighbors)
    val activeNeighbors = neighbors.count(_ == 1)
    println(p + " : " + activeNeighbors)
    if ((p == 1 && activeNeighbors >= 2 && activeNeighbors <= 3) || (p == 0 && activeNeighbors == 3)) {
      println("active")
      return 1
    }
    else return 0
  }

  private def part2(data: InputType): Long =
    0

  private val testInput =
    """.#.
      |..#
      |###
      |""".stripMargin

  private val input =
    """##.#####
      |#.##..#.
      |.##...##
      |###.#...
      |.#######
      |##....##
      |###.###.
      |.#.#.#..
      |""".stripMargin
