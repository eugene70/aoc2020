package aoc2020

import scala.annotation.tailrec

object Day22:
  val emptyLinePattern = """\n\s*\n"""
  type InputType = (CardDeck, CardDeck)
  type CardDeck = Seq[Int]

  @main def runDay22: Unit =
    val testData = time("testReady", () => ready(testInput))
    time("testPart1", () => part1(testData)) // should be 306
    time("testPart2", () => part2(testData)) // shuld be 291

    time("recusivePart1", () => part1(ready(recursiveInput))) // should be 145

    val data = time("ready", () => ready(input))
    time("part1", () => part1(data))
    time("part2", () => part2(data))

  def ready(input: String): InputType =
    val list = input
      .split(emptyLinePattern)
      .map(_.linesIterator.drop(1).map(_.toInt).toSeq)
      .toList
    (list(0), list(1))

  def part1(data: InputType): Long =
    eval(round(0, data, data))

  @tailrec
  def round(depth: Long, orgDecks: InputType, decks: InputType): InputType =
    if (depth > 1000 || (depth > 0 && orgDecks == decks)) {
      return (decks._1.tail :+ decks._1.head :+ decks._2.head, decks._2.tail)
    }
    if (decks._1.length == 0 || decks._2.length == 0) return decks
    round(depth + 1, orgDecks,
      if (decks._1.head > decks._2.head) (decks._1.tail :+ decks._1.head :+ decks._2.head, decks._2.tail)
      else (decks._1.tail, decks._2.tail :+ decks._2.head :+ decks._1.head)
    )

  def eval(decks: InputType): Long =
    val winDeck = if (decks._1.length > 0) decks._1 else decks._2
    eval(winDeck)

  def eval(deck: CardDeck): Long =
    deck.reverse.zipWithIndex.map((card, index) => card * (index + 1)).sum

  @tailrec
  def round2(decks: InputType): InputType =
    if (decks._1.length == 0 || decks._2.length == 0) return decks
    round2(
      if (winFirst(decks)) (decks._1.tail :+ decks._1.head :+ decks._2.head, decks._2.tail)
      else (decks._1.tail, decks._2.tail :+ decks._2.head :+ decks._1.head)
    )

  def winFirst(decks: InputType) =
    if (decks._1.head < decks._1.length && decks._2.head < decks._2.length) {
      subgame((decks._1.drop(1).take(decks._1.head), decks._2.drop(1).take(decks._2.head)))
    }
    else
      decks._1.head > decks._2.head

  def subgame(decks: InputType): Boolean =
    round(0, decks, decks)._1.length > 0

  def part2(data: InputType): Long =
    eval(round2(data))

  val testInput =
    """Player 1:
      |9
      |2
      |6
      |3
      |1
      |
      |Player 2:
      |5
      |8
      |4
      |7
      |10
      |""".stripMargin

  val recursiveInput =
    """Player 1:
      |43
      |19
      |
      |Player 2:
      |2
      |29
      |14""".stripMargin

  val input =
    """Player 1:
      |28
      |13
      |25
      |16
      |38
      |3
      |14
      |6
      |29
      |2
      |47
      |20
      |35
      |43
      |30
      |39
      |21
      |42
      |50
      |48
      |23
      |11
      |34
      |24
      |41
      |
      |Player 2:
      |27
      |37
      |9
      |10
      |17
      |31
      |19
      |33
      |40
      |12
      |32
      |1
      |18
      |36
      |49
      |46
      |26
      |4
      |45
      |8
      |15
      |5
      |44
      |22
      |7
      |""".stripMargin
