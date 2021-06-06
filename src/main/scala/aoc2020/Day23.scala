package aoc2020

import scala.annotation.tailrec

object Day23:
  type InputType = Seq[Long]
  type Label = Long

  class Cup(val label: Label):
    var next = this
    def link(next: Cup) =
      this.next = next

  class CrabCups(val head: Cup, val index: Map[Label, Cup]):
    var current = head
    def next = current.next
    def pickup =
      val head = current.next
      current.link(current.next.next.next.next)
      head.next.next.link(head)
      head
    def destinationLabel(picked: Cup) =
      val nums = (1 to 4).map(current.label - _)
                         .map(n => if (n > 0) n else index.size + n)
      nums.filter(n => n != picked.label && n != picked.next.label && n != picked.next.next.label)
        .head
    def insertNext(label: Label, picked: Cup) =
      val aCup = index.get(label).head
      val nextCup = aCup.next
      aCup.link(picked)
      picked.next.next.link(nextCup)
    def move =
      val picked = pickup
      val destLabel = destinationLabel(picked)
      insertNext(destLabel, picked)
      current = next
    def find(label: Label): Cup =
      index.get(label).head
    def toSeq(fromLabel: Label): Seq[Label] =
      var head = index.get(fromLabel).head
      var seq = Seq.empty[Label]
      (1 to index.size).foreach(n => {
        seq = seq :+ head.label
        head = head.next
      })
      seq
  object CrabCups:
    def of(labels: Seq[Label]) =
      val cups = labels.map(Cup(_)).toSeq
      val map = cups.map(c => (c.label, c)).toMap
      cups.sliding(2).foreach(c => c.head.link(c.last))
      cups.last.link(cups.head)
      CrabCups(cups.head, map)

  @main def runDay23: Unit =
    val testData = time("testReady", () => ready(testInput))
    time("testPart1", () => part1(testData)) // should be 67384529
    time("testPart2", () => part2(testData)) // should be 149245887792

    val data = time("ready", () => ready(input))
    time("part1", () => part1(data)) // 38925764
    time("part2", () => part2(data)) // 131152940564

  private def ready(input: String): InputType =
    input.map(_.toLong - 48).toIndexedSeq

  def destination(curr: Long, pickup: Seq[Long], max: Long) =
    val candidate = (1 to 4).map(curr - _).map(n => if (n > 0) n else max + n)
    candidate.find(pickup.indexOf(_) < 0).head

  private def part1(data: InputType) =
    val game = CrabCups.of(data)
    (1 to 100).foreach(_ => game.move)
    game.toSeq(1).tail.mkString("")

  private def part2(data: InputType) =
    val game = CrabCups.of(data :++ (data.max + 1 to 1_000_000))
    (1 to 10_000_000).foreach(_ => game.move)
    game.find(1).next.label * game.find(1).next.next.label

  private val testInput = "389125467"

  private val input = "562893147"
