package aoc2020

object Day13:
  type InputType = (Int, List[String])

  def ready(input: String): InputType =
    val in = input
      .linesIterator
      .toList
    (in(0).toInt, in(1).split(',').toList)

  @main def solveDay13: Unit =
    val testData = time("testReady", () => ready(testInput))
    time("testPart1", () => part1(testData)) // should be 295
    time("testPart2", () => part2(testData)) // should be 1068781

    val data = time("ready", () => ready(input))
    time("part1", () => part1(data))
    time("part2", () => part2(data))

  /**
   * 각 버스 번호의 배수 중 주어진 timestamp 수 보다 큰 수 중 가장 작은 수를 구하는 문제
   *
   * @param data
   * @return
   */
  def part1(implicit data: InputType): Long =
    val timestamp = data._1
    val busList = data._2
    val nums = busList.filter(_ != "x").map(_.toInt)
    val min = 
      nums
        .zip(nums.map(busNum => (timestamp / busNum + 1) * busNum - timestamp))
        .minBy(_._2)
    min._1 * min._2

  /**
   * 두 수의 최소 공배수 구하기
   *
   * @param a 수1
   * @param b 수2
   * @return 최소 공배수(LCM)
   */
  def lcm(a: Long, b: Long): Long =
    Math.abs(a * b) / gcd(a, b)

  /**
   * 두 수의 최대 공약수 구하기
   * @param a 수1
   * @param b 수2
   * @return 최대 공약수(GCD)
   */
  def gcd(a: Long, b: Long): Long =
    val r = a % b
    if (r == 0) return b
    else gcd(b, r)

  @main def testDay13: Unit =
    val testData = ready(testInput)
    testData._2
      .zipWithIndex
      .filter(_._1 != "x")
      .map(t => (BigInt(t._1), t._2))
      .foreach(println)

  def part2_2(implicit data: InputType): Long =
    data._2
      .zipWithIndex
      .filter(_._1 != "x")
      .map(t => t._1.toLong)
      .reduce(lcm)

  def part2(implicit data: InputType): Long =
    val busList = data._2
    val sorted = busList
      .zipWithIndex
      .filter(_._1 != "x")
      .map(t => (t._1.toLong, t._2))
      .sortBy(_._1)
      .reverse
      .toIndexedSeq
    var i: Long = 1L
    while (true) {
      val timestamp = sorted(0)._1 * i - sorted(0)._2
      //println (timestamp)
      if (sorted.forall(bus => (timestamp + bus._2) % bus._1 == 0)) return timestamp
      i += 1
    }
    0L

  private val testInput =
    """939
      |7,13,x,x,59,x,31,19
      |""".stripMargin

  private val input =
    """1001938
      |41,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,431,x,x,x,x,x,x,x,23,x,x,x,x,13,x,x,x,17,x,19,x,x,x,x,x,x,x,x,x,x,x,863,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29
      |""".stripMargin
