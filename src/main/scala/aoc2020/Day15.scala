package aoc2020

import scala.annotation.tailrec

object Day15:
  type InputType = Seq[Int]

  @main def runDay15: Unit =
    val testData = time("testReady", () => ready(testInput))
    time("tesPart1", () => part1(testData)) // would be 436
    time("testPart2", () => part2(testData)) // would be 175594

    val data = time("ready", () => ready(input))
    time("part1", () => part1(data))
    time("part2", () => part2(data))

  private def ready(input: String): InputType =
    input
      .split(',')
      .map(_.trim.toInt)
      .toSeq

  def part1(data: InputType): Long =
    @tailrec
    def next(data: InputType): InputType =
      if (data.size == 2020) return data
      next(data :+ (
        data match {
          case nums :+ n =>
            val i = nums.lastIndexOf(n)
            if (i == -1) then 0
            else data.size - i - 1
        }
        ))
    next(data).last
  
  case class Memory(lastIndex: Int, lastNumber: Int)
  
  // 약 5초 정도 소요, 성능을 위해 mutalbe map을 사용, imutable map 은 15초 정도 소요
  def part2org(data: InputType): Long =
    val initialMap = data.init.zipWithIndex.toMap
    val mutableMap = collection.mutable.Map[Int, Int]().addAll(initialMap)
    val initalMemory = Memory(data.size - 1, data.last)
    Range(data.size, 30_000_000)
      .foldLeft
        (initalMemory)
        ((mem, index) => {
          val result = Memory(
            index,
            mutableMap.get(mem.lastNumber) match
              case Some(i) => mem.lastIndex - i
              case _ => 0
          )
          mutableMap.put(mem.lastNumber, mem.lastIndex)
          result
        })
      .lastNumber

  // 속도를 위하여 Map 대신 큰 정수 배열을 이용하여 검색한다.
  def part2(data: InputType): Long =
    val indexTable = Array.fill(30_000_000)(-1)
    data.init.zipWithIndex.foreach((n, i) => indexTable(n) = i)
    val initalMemory = Memory(data.size - 1, data.last)
    Range(data.size, 30_000_000)
      .foldLeft(initalMemory)
        ((mem, index) => {
          val result = Memory(
            index,
            indexTable(mem.lastNumber) match
              case -1 => 0
              case i => mem.lastIndex - i
          )
          indexTable(mem.lastNumber) = mem.lastIndex
          result
        })
      .lastNumber
    0

  def part2x(data: InputType): Long =
    val arr = Range(0, 30_000_000).zipAll(data, 0, 0).map((i, v) => v).toArray
    Range(data.size, 30_000_000)
      .foreach(i =>
        arr(i) = {
          val f = arr.lastIndexOf(arr(i - 1), i - 2)
          if (f == -1) then 0
          else i - f - 1
        }
      )
      arr.last

  private val testInput =
    """0,3,6
      |""".stripMargin

  private val input =
    """6,3,15,13,1,0
      |""".stripMargin
