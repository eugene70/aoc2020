package aoc2020

object Day20:
  type InputType = IndexedSeq[Tile]

  class Image(val pixels: IndexedSeq[IndexedSeq[Boolean]]):
    def this(image: String) =
      this(
        image.linesIterator
          .filter(!_.trim.isEmpty)
          .map(_.map(_ == '#').toIndexedSeq)
          .toIndexedSeq
      )
    def stripEdges =
      Array(
        pixels(0),
        pixels(pixels.size - 1),
        pixels.map(_(0)).toIndexedSeq,
        pixels.map(_(pixels(0).size - 1)).toIndexedSeq
      ).toIndexedSeq
    def hasSameSide(other: Image) =
      this.stripEdges.exists(thisEdge => other.stripEdges.exists(_ == thisEdge))
      || this.stripEdges.exists(thisEdge => other.stripEdges.map(_.reverse).exists(_ == thisEdge))

    override def toString() =
      pixels.toString()

  case class Tile(no: Int, image: Image)

  @main def runDay20: Unit =
    val testData = time("testReady", () => ready(testInput))
    time("tesPart1", () => part1(testData)) // should be 20899048083289
    time("testPart2", () => part2(testData)) // should be 273
//
    val data = time("ready", () => ready(input))
    time("part1", () => part1(data))
//    time("part2", () => part2(data))

  private def ready(input: String): InputType =
    input
      .split("""\n\s*\n""")
      .map(_ match
        case s"Tile $no:$image" => new Tile(no.toInt, Image(image))
      )
      .toIndexedSeq

  private def part1(data: InputType) =
    println(data(0).no)
    println(data(0).image)
    println(data(0).image.stripEdges)
    data.map(tile => (tile.no, data.count(_.image.hasSameSide(tile.image))))
      .filter(_._2 == 3).map(_._1).map(BigInt(_)).product

  private def part2(data: InputType): BigInt =
    // TODO implemantation
    0

  val monsterPattern =
    """                  #
      |#    ##    ##    ###
      | #  #  #  #  #  #   """.stripMargin

  private val testInput =
    """Tile 2311:
      |..##.#..#.
      |##..#.....
      |#...##..#.
      |####.#...#
      |##.##.###.
      |##...#.###
      |.#.#.#..##
      |..#....#..
      |###...#.#.
      |..###..###
      |
      |Tile 1951:
      |#.##...##.
      |#.####...#
      |.....#..##
      |#...######
      |.##.#....#
      |.###.#####
      |###.##.##.
      |.###....#.
      |..#.#..#.#
      |#...##.#..
      |
      |Tile 1171:
      |####...##.
      |#..##.#..#
      |##.#..#.#.
      |.###.####.
      |..###.####
      |.##....##.
      |.#...####.
      |#.##.####.
      |####..#...
      |.....##...
      |
      |Tile 1427:
      |###.##.#..
      |.#..#.##..
      |.#.##.#..#
      |#.#.#.##.#
      |....#...##
      |...##..##.
      |...#.#####
      |.#.####.#.
      |..#..###.#
      |..##.#..#.
      |
      |Tile 1489:
      |##.#.#....
      |..##...#..
      |.##..##...
      |..#...#...
      |#####...#.
      |#..#.#.#.#
      |...#.#.#..
      |##.#...##.
      |..##.##.##
      |###.##.#..
      |
      |Tile 2473:
      |#....####.
      |#..#.##...
      |#.##..#...
      |######.#.#
      |.#...#.#.#
      |.#########
      |.###.#..#.
      |########.#
      |##...##.#.
      |..###.#.#.
      |
      |Tile 2971:
      |..#.#....#
      |#...###...
      |#.#.###...
      |##.##..#..
      |.#####..##
      |.#..####.#
      |#..#.#..#.
      |..####.###
      |..#.#.###.
      |...#.#.#.#
      |
      |Tile 2729:
      |...#.#.#.#
      |####.#....
      |..#.#.....
      |....#..#.#
      |.##..##.#.
      |.#.####...
      |####.#.#..
      |##.####...
      |##..#.##..
      |#.##...##.
      |
      |Tile 3079:
      |#.#.#####.
      |.#..######
      |..#.......
      |######....
      |####.#..#.
      |.#...#.##.
      |#.#####.##
      |..#.###...
      |..#.......
      |..#.###...
      |""".stripMargin

  private val input =
    """Tile 2953:
      |.###.###..
      |.#....#..#
      |#.#......#
      |.###.#..##
      |#..##..#..
      |....#....#
      |.##.#..#.#
      |#......#..
      |#.........
      |..##..#.#.
      |
      |Tile 2053:
      |##.##...#.
      |......#.#.
      |..###.#...
      |..#..#.###
      |##.......#
      |###...#.##
      |.#.##.##..
      |##...#...#
      |##.##..###
      |.#...####.
      |
      |Tile 1213:
      |.......#..
      |#...##...#
      |....#..#..
      |..#..###..
      |#..##..#..
      |#.#.#.....
      |...#.#..##
      |#.#.#.#...
      |....#.....
      |..##....##
      |
      |Tile 2801:
      |.#.##.#...
      |#.#....###
      |.....##..#
      |.....#....
      |#....##...
      |#.#...#..#
      |..#..#..##
      |#........#
      |#..#...#..
      |..###.....
      |
      |Tile 1109:
      |#######...
      |..........
      |##..#....#
      |#........#
      |.#.#...#..
      |####.....#
      |...#.##...
      |.##....#.#
      |#..##...#.
      |#.#..##...
      |
      |Tile 2833:
      |#.#.#.#..#
      |#.#...#..#
      |.....#.#.#
      |#.##....##
      |#.##..##.#
      |#.#..##.#.
      |..#......#
      |#....#....
      |..#.#.#...
      |#..##..##.
      |
      |Tile 2099:
      |##.#..#..#
      |.....#..#.
      |.........#
      |.....###..
      |#.........
      |####.#..#.
      |######...#
      |##.##.#...
      |.....###..
      |.....##..#
      |
      |Tile 1867:
      |#..####.#.
      |#....#..##
      |#..##.#..#
      |..#..#...#
      |#...#....#
      |...##.....
      |..........
      |....##.###
      |##..#.#.##
      |##..#.####
      |
      |Tile 2551:
      |#.#.#...#.
      |#....#...#
      |.#.#.....#
      |#.#.#...#.
      |#.#...#.##
      |...#.##..#
      |...#.....#
      |#.........
      |#..#.#....
      |##..#.##.#
      |
      |Tile 2239:
      |#.#...###.
      |#.#.....##
      |##....#...
      |##.......#
      |.#.#..#..#
      |###......#
      |#.........
      |...#....##
      |#..#.##..#
      |...###....
      |
      |Tile 3049:
      |..##....#.
      |...##...##
      |....###..#
      |.#.##.....
      |.##...#.#.
      |..#...#..#
      |#......#..
      |.####...##
      |#..##.....
      |.##.##.###
      |
      |Tile 1913:
      |###....#..
      |.....##...
      |#.#.....#.
      |....#....#
      |#...#...#.
      |.......###
      |..#....#..
      |##........
      |..#...#..#
      |##....#.#.
      |
      |Tile 3469:
      |.##.##...#
      |#...####..
      |.###.##...
      |##.#...#..
      |#...#...##
      |#.##.#..##
      |##......##
      |..........
      |#.....#..#
      |#.##..##..
      |
      |Tile 2467:
      |##.#.###..
      |..#####..#
      |#....##...
      |#.....#.##
      |#.#...#...
      |.....##.#.
      |#..#..#..#
      |#....##..#
      |#..###.#..
      |.##.......
      |
      |Tile 3467:
      |##..##....
      |##......#.
      |##....#...
      |.#...##...
      |#.#....#..
      |##....#.##
      |#..#..#.#.
      |#.....####
      |#.#..####.
      |.#.#.#..#.
      |
      |Tile 2683:
      |#..##...#.
      |.####....#
      |#.......##
      |#.....#.##
      |...##.#..#
      |##.#......
      |##....#...
      |.##.....#.
      |..#......#
      |##.....##.
      |
      |Tile 2927:
      |.#..#.####
      |#..##....#
      |..##.###.#
      |..###..##.
      |##.....##.
      |#.........
      |.#..#..#.#
      |.###....#.
      |#..#..#...
      |##..##....
      |
      |Tile 3037:
      |..###...#.
      |#..#.....#
      |.####....#
      |#....###.#
      |..#....#.#
      |#.##...#..
      |......#..#
      |.##..##...
      |..#..#..#.
      |.##....###
      |
      |Tile 1019:
      |.##..#...#
      |...#..#.##
      |###.##..#.
      |.##..#....
      |#.#...#...
      |....##...#
      |..##.#.#.#
      |#.....####
      |......####
      |..###....#
      |
      |Tile 3089:
      |#..##..##.
      |..##......
      |.#....#...
      |..#..#....
      |.......#.#
      |...##.....
      |#.#...#...
      |#.......#.
      |#........#
      |.##...#..#
      |
      |Tile 2011:
      |...#.###.#
      |#.#..#.#.#
      |#.#..###..
      |.......##.
      |...#.....#
      |.#.......#
      |##..#..#..
      |...#..#...
      |###....##.
      |...#.#...#
      |
      |Tile 1801:
      |..####...#
      |#..#..#..#
      |#...#...#.
      |....#....#
      |..##.....#
      |....#.....
      |#..##.....
      |#.###.....
      |....##....
      |..#.#.#...
      |
      |Tile 1217:
      |#.#.#.##..
      |##.#...#.#
      |.....#....
      |..#.....#.
      |#...##...#
      |..#.#..###
      |#..#..#..#
      |.#........
      |#......#.#
      |#...#.##.#
      |
      |Tile 1789:
      |.#.#####..
      |..#.#....#
      |.##..#..#.
      |..#.###.##
      |..##......
      |.#.#..#...
      |#...#.#...
      |..........
      |#.##......
      |#.#..#..##
      |
      |Tile 3719:
      |..####.##.
      |###.......
      |...#..#.#.
      |.......#..
      |#.###....#
      |........##
      |#.....#...
      |...##..#.#
      |#.#....##.
      |#.#..##.#.
      |
      |Tile 3637:
      |#....#..#.
      |##..#..#..
      |#.....#..#
      |#.........
      |##........
      |#.###....#
      |#..##....#
      |..#.#...#.
      |......###.
      |..#.#..##.
      |
      |Tile 1471:
      |.##.#..#..
      |.##....#.#
      |###...##.#
      |..##.##.##
      |..........
      |#......###
      |#.#.#.#..#
      |.....#.###
      |#.#..#...#
      |...#####..
      |
      |Tile 1933:
      |##.###....
      |#..#....#.
      |#..#..##.#
      |.#..#..###
      |.....##...
      |.##..#...#
      |....#.#..#
      |#..#......
      |#.##.#.##.
      |....###.#.
      |
      |Tile 1381:
      |.##..##.#.
      |.#..###..#
      |##.#.##.##
      |.#..#.....
      |#...##...#
      |.#.......#
      |#........#
      |####.....#
      |#.#...####
      |.#..#..###
      |
      |Tile 2423:
      |#.#.######
      |#.........
      |##....##.#
      |#....#.#..
      |..#..#....
      |......#..#
      |##..#..#..
      |....###...
      |#....#....
      |..#.##....
      |
      |Tile 3373:
      |.#.#..####
      |#.#.#....#
      |#..#.....#
      |....#....#
      |#..#..#.##
      |.....##...
      |#...##..##
      |..#.....#.
      |.#.#....##
      |##..##.#.#
      |
      |Tile 1163:
      |###.....##
      |#...#....#
      |##.......#
      |.........#
      |...###.#.#
      |..#.......
      |...##.#..#
      |.#.#...##.
      |..#.##....
      |######.###
      |
      |Tile 2543:
      |#...#.##.#
      |#.#......#
      |....#.#..#
      |##......##
      |##..#...##
      |..........
      |#.....##.#
      |###..#.###
      |.####.#..#
      |##...##...
      |
      |Tile 1427:
      |#.#....##.
      |#.......##
      |...#.###.#
      |...#......
      |..#..#....
      |.#.....#..
      |..##.....#
      |#.#.#...#.
      |..#.#.#.##
      |.#...##.##
      |
      |Tile 3371:
      |.##...#..#
      |#...#..#..
      |.#........
      |..#...####
      |..###.#..#
      |##.#....#.
      |.##.#.#.##
      |#..####...
      |....#..###
      |###.#...#.
      |
      |Tile 3529:
      |.#.#..#..#
      |##..#..#..
      |..........
      |#..#..##..
      |###....#.#
      |.#....##.#
      |#.#......#
      |...#.#.#.#
      |#.........
      |.#.#..#.##
      |
      |Tile 3539:
      |###.#..##.
      |##.#...###
      |#..#...###
      |#..#......
      |...##..#.#
      |#...#.#...
      |#.#.##....
      |#.#..#..##
      |#..#.##.##
      |##.#.#....
      |
      |Tile 1511:
      |#..#####..
      |.##..#..#.
      |#.....#..#
      |#.#..##..#
      |..#......#
      |.#........
      |#.#.#.#..#
      |##.#.#..#.
      |##........
      |....#.....
      |
      |Tile 2521:
      |.###....#.
      |#........#
      |.......#..
      |.#....#..#
      |##....#..#
      |......#...
      |...#.#..##
      |#......###
      |#...#....#
      |....###..#
      |
      |Tile 1637:
      |.#..#.....
      |#.......#.
      |...#..#.##
      |#......#..
      |#.###.#..#
      |.#...##...
      |..###.#...
      |##...#.###
      |..#..##.#.
      |.##.#..#.#
      |
      |Tile 2351:
      |####.###..
      |##...###..
      |#....#..#.
      |#........#
      |#.#.......
      |##..##...#
      |####..#...
      |##...##.#.
      |##...#.#..
      |..#####.#.
      |
      |Tile 3929:
      |.#..##...#
      |.#.#..#...
      |##..##.#.#
      |..#...#...
      |.##.#...#.
      |#.....#..#
      |...#..#..#
      |##...#....
      |##......#.
      |.##..##...
      |
      |Tile 1223:
      |....#.....
      |..#..#....
      |..#...#...
      |...#....##
      |..#......#
      |#.....#...
      |..#..##...
      |....#.....
      |#.....##.#
      |..##.#....
      |
      |Tile 1009:
      |#....###.#
      |#.#......#
      |....##..#.
      |#.........
      |#..##....#
      |.##.......
      |.....#...#
      |.......##.
      |.....#.#.#
      |.#.....##.
      |
      |Tile 1289:
      |.##..#.#.#
      |.......##.
      |.#...#...#
      |....#.....
      |........##
      |#...#..#..
      |#....#...#
      |#....###.#
      |#..#....#.
      |###..#....
      |
      |Tile 3943:
      |.#.#.#....
      |#.....#.#.
      |#....#...#
      |....#.#.#.
      |....#..#..
      |##.#.#.#..
      |##...#.#..
      |#.........
      |.#....##.#
      |#.#.#####.
      |
      |Tile 2633:
      |.####..##.
      |....#.....
      |....###...
      |......#..#
      |....#.....
      |#..#.#...#
      |#.##...#.#
      |##..#.##.#
      |..#.....##
      |#.#.####..
      |
      |Tile 2797:
      |.###..#.#.
      |#...#.##.#
      |#.....#..#
      |....#...#.
      |...#....#.
      |.##.#.....
      |......##..
      |....#....#
      |#.........
      |..#...###.
      |
      |Tile 1973:
      |....#.##.#
      |##.....#.#
      |..#..#.#.#
      |#.#.#..#..
      |.....##.##
      |.##.#.#...
      |..###.#...
      |........##
      |##...#...#
      |..#...#..#
      |
      |Tile 2621:
      |#.##.##.##
      |.#..#.#.##
      |........#.
      |#...##....
      |.####..#.#
      |....#....#
      |.#........
      |...#....##
      |..##.#.#.#
      |###.##....
      |
      |Tile 1303:
      |..########
      |.#.#......
      |#.#..#..#.
      |.#.#.....#
      |...##..#..
      |..##......
      |.#.#...#..
      |#.....##..
      |#........#
      |#.##.#.#..
      |
      |Tile 1483:
      |.##.......
      |.#.#.##.#.
      |.##...#...
      |##...#..##
      |.#.#..#.##
      |#...#.##..
      |.##...#...
      |.......#.#
      |.#........
      |.#..#.##..
      |
      |Tile 1229:
      |#.#.##..##
      |..#.##.##.
      |#####.###.
      |.#.#.....#
      |....##..#.
      |...#.##.#.
      |#.....#..#
      |#.##......
      |.........#
      |#.###....#
      |
      |Tile 1319:
      |#..#..##.#
      |##..#....#
      |..#....##.
      |#.#...#..#
      |...#.....#
      |#.#..#.#..
      |##....#..#
      |###.....#.
      |..#.#..#.#
      |.#..##....
      |
      |Tile 2837:
      |#.##.#.##.
      |.##.#.....
      |##..#.....
      |#...##..#.
      |.#.#...#.#
      |..#...#..#
      |..#..#...#
      |..##..#...
      |...##....#
      |.#.#..##.#
      |
      |Tile 2503:
      |##...#####
      |##...#....
      |....#....#
      |..#####...
      |##.#.##.#.
      |#....#.#.#
      |#.#....#..
      |........##
      |.#..#.....
      |#..####.##
      |
      |Tile 2711:
      |.##.#.##..
      |.#.....#.#
      |......#..#
      |##..#...##
      |..#....###
      |.##.#.###.
      |....#..#.#
      |......#...
      |#....#...#
      |#..####.#.
      |
      |Tile 2137:
      |#.......##
      |#.#.......
      |#.#.#.....
      |#.....#..#
      |#...##.#.#
      |#..#...#.#
      |#..#.#....
      |..#.#.#...
      |#.##......
      |#.####.#.#
      |
      |Tile 1543:
      |#.....#.##
      |..##.....#
      |#..#......
      |####....#.
      |#.#.#..#.#
      |..#..#....
      |....#..#..
      |###..##..#
      |..##.#.###
      |##.#....##
      |
      |Tile 2161:
      |..#...##..
      |##...####.
      |#.##.#.##.
      |##........
      |#.....##.#
      |..#.#.##..
      |#...#.....
      |#.#..#..##
      |...##..###
      |.#..###.#.
      |
      |Tile 2377:
      |...######.
      |#.....#..#
      |#...##....
      |#.....##..
      |.#......#.
      |.#.#..##..
      |......#..#
      |.......#..
      |..#.....#.
      |..###.#.#.
      |
      |Tile 1429:
      |.#.##.#..#
      |.#....##..
      |.#...##..#
      |.#...#...#
      |...##.###.
      |.......###
      |##......##
      |#..#..##.#
      |#....##..#
      |#..###..##
      |
      |Tile 2113:
      |#.######..
      |..#...##..
      |#.####.#.#
      |..........
      |..##......
      |....#.###.
      |#....#...#
      |#...##....
      |......##..
      |.######...
      |
      |Tile 2677:
      |#..#.#####
      |......#..#
      |....##...#
      |....#.....
      |.##.##....
      |#......##.
      |...###.#.#
      |####.#..#.
      |..#...#...
      |##.#####..
      |
      |Tile 2591:
      |.#.##..##.
      |##.#......
      |.#..#.#.##
      |.##..##..#
      |.........#
      |#..#.#.##.
      |........#.
      |...#..#...
      |.....#..#.
      |.#.####...
      |
      |Tile 2287:
      |..##...##.
      |##.###....
      |#....#..##
      |#.#.....##
      |#.......#.
      |#...#....#
      |...#..#.##
      |.#..##...#
      |#....#.##.
      |.####..#..
      |
      |Tile 1279:
      |#..##.#..#
      |##........
      |...##.....
      |##...#...#
      |#..#.....#
      |.......#.#
      |..#..#...#
      |...#.#.##.
      |#..#....#.
      |#.#...##..
      |
      |Tile 3769:
      |##.....##.
      |....#....#
      |...#.#....
      |....#....#
      |#....#...#
      |#..#..##.#
      |#......#.#
      |..........
      |.....#...#
      |##.#.##..#
      |
      |Tile 1931:
      |.##.#.##.#
      |..###..#.#
      |#.##..#...
      |..#..#.#..
      |..#...####
      |#.....#.#.
      |..........
      |..####..##
      |..#....###
      |....#.##.#
      |
      |Tile 3847:
      |.#.####...
      |.......#.#
      |##...#...#
      |#...##...#
      |....#....#
      |..#..#....
      |#.#.#..#..
      |..#..#....
      |##...#....
      |##..##.#..
      |
      |Tile 2549:
      |..##....#.
      |#.........
      |#....#....
      |#.....#...
      |#.##...#..
      |##...#.#.#
      |.####..#..
      |......####
      |...####.##
      |..#####..#
      |
      |Tile 3067:
      |..####..##
      |#....#....
      |##.....#.#
      |....#...##
      |........#.
      |...#...###
      |.......#.#
      |#..##..##.
      |..##...#.#
      |...#...##.
      |
      |Tile 2083:
      |.####..#..
      |##.#..#..#
      |.......#..
      |##..##.#.#
      |.......##.
      |##....###.
      |#....#...#
      |#...##...#
      |...#......
      |.####.#..#
      |
      |Tile 2819:
      |...##..###
      |#.##.....#
      |##.##.#..#
      |.....#..##
      |#......#.#
      |#........#
      |##....####
      |#.....#.#.
      |#....#...#
      |....#...#.
      |
      |Tile 2089:
      |#####.#.#.
      |###..#..#.
      |#.....#...
      |.####..#..
      |..####..#.
      |#....#..#.
      |....#.#...
      |....#.#.##
      |#.#.......
      |..####.#..
      |
      |Tile 1459:
      |..##..#..#
      |.##..#..#.
      |...#......
      |.....##.#.
      |.#.#.#....
      |.....#...#
      |.....##...
      |.#.#..####
      |#...##...#
      |.#.#.#.#..
      |
      |Tile 3761:
      |..#.#...#.
      |...#.#####
      |.#...#..##
      |#..#.#....
      |..........
      |..#..#..##
      |#.####..#.
      |....####..
      |##.#..#...
      |#.##.#####
      |
      |Tile 2707:
      |######.###
      |##..#.##.#
      |.#.......#
      |...#...#..
      |#.....#...
      |...#......
      |........#.
      |#...#.....
      |.###..#..#
      |.....#.##.
      |
      |Tile 2371:
      |.###..####
      |###......#
      |......#...
      |...##..#.#
      |...#.....#
      |##......#.
      |...#..##.#
      |#.....##..
      |##..##.#..
      |##...#.#..
      |
      |Tile 1907:
      |.#.#.####.
      |#..#.....#
      |#..##.....
      |##..#.###.
      |#...#.#.#.
      |..#.#.....
      |#.##......
      |###......#
      |#........#
      |#.#.#....#
      |
      |Tile 1499:
      |####...#..
      |...#......
      |.....#....
      |##..#....#
      |#...##...#
      |#.#....#..
      |.....#.###
      |#......#.#
      |###...#.##
      |##..##...#
      |
      |Tile 2267:
      |.#..##...#
      |#...#.#..#
      |##.....#.#
      |..........
      |###.##....
      |.####..#.#
      |#........#
      |......#.#.
      |#.#..#.#.#
      |..#.#...##
      |
      |Tile 1283:
      |######..##
      |##.#.....#
      |#.......##
      |#...##...#
      |##........
      |#.....#..#
      |#....#...#
      |#..###.#..
      |#.........
      |...##.....
      |
      |Tile 3499:
      |##..#..##.
      |#........#
      |#....#....
      |#.####....
      |#.#......#
      |....##...#
      |#..#....#.
      |#...#....#
      |#.#....##.
      |.#.#.#.#..
      |
      |Tile 3881:
      |#..#....##
      |#..#.....#
      |..#...#..#
      |#...##...#
      |#.......##
      |..#..#####
      |....#...#.
      |##..##....
      |#.#..#....
      |..##.###.#
      |
      |Tile 2341:
      |...##.#.##
      |...#.##.##
      |.##..###.#
      |#..#######
      |##..#..#.#
      |......##.#
      |....#.##.#
      |####.#..##
      |##....###.
      |.##.#..#.#
      |
      |Tile 2399:
      |......####
      |#...#.....
      |..#...#.##
      |.#.#....##
      |##........
      |##.....##.
      |.....#...#
      |#....#.##.
      |#...#..#..
      |..#.#.###.
      |
      |Tile 3697:
      |..#..##..#
      |#.###..#.#
      |.#.#.#...#
      |#.#.#....#
      |#.###.#..#
      |......##.#
      |##.#....##
      |.#......#.
      |####.#...#
      |#.#..#.###
      |
      |Tile 3331:
      |#.#.##....
      |##.....###
      |####......
      |.#.##.#..#
      |.##.......
      |#.#..#...#
      |.....#.#..
      |#.##.#.##.
      |###......#
      |.#.#...##.
      |
      |Tile 1979:
      |...###..##
      |..#..#..##
      |...##....#
      |###...#.#.
      |#.....#..#
      |..##..#...
      |.....#.#..
      |#..#......
      |##........
      |#####.....
      |
      |Tile 3041:
      |#...##.#.#
      |#..###.#.#
      |...#...###
      |#...#..#.#
      |.#..##...#
      |#.....###.
      |......#...
      |.#.#.#...#
      |..#.##..##
      |...#..#.##
      |
      |Tile 1307:
      |.#.#.#...#
      |.##...##.#
      |...#.....#
      |#.....#...
      |#........#
      |##.###.#.#
      |##....#..#
      |#........#
      |.###....##
      |##..#.###.
      |
      |Tile 1523:
      |..#.....##
      |.....##...
      |#........#
      |#.#.#...#.
      |#..#..#.##
      |#.......##
      |..#..#.#.#
      |....#....#
      |###......#
      |...#.#..#.
      |
      |Tile 3491:
      |####..#...
      |.#....#.##
      |.#..#..#.#
      |#...#..###
      |#.......#.
      |#.....#..#
      |.....#.#..
      |#.#..##...
      |.#...#...#
      |##.#..#.#.
      |
      |Tile 3989:
      |####....##
      |##..###..#
      |#..#....##
      |...#####.#
      |###..#..##
      |#.####..#.
      |##..#....#
      |..#......#
      |...##...##
      |.##.#..###
      |
      |Tile 3727:
      |#.#..#.#..
      |#.#......#
      |......#..#
      |..#....###
      |#..##.#...
      |...#..#...
      |#..##.####
      |#..#.....#
      |.......#..
      |...###..##
      |
      |Tile 1583:
      |..##....##
      |#....##...
      |##..#...#.
      |....##.#..
      |#...###..#
      |.##.#.....
      |....#..#..
      |...#.#....
      |....##..##
      |#..##...#.
      |
      |Tile 3581:
      |##..#.....
      |....#..##.
      |##..#....#
      |..#.......
      |#.....#...
      |#.....#...
      |#...#....#
      |.........#
      |.....##..#
      |##..#.####
      |
      |Tile 3559:
      |.#........
      |....##....
      |....#..#.#
      |##..#.##..
      |#..#.#....
      |..##.#...#
      |.#..##..##
      |....#..#..
      |##.#.....#
      |#....#####
      |
      |Tile 2131:
      |..########
      |#.........
      |..#.##..#.
      |.#.#.#....
      |####....##
      |#####.....
      |#.........
      |#.#.....#.
      |..##....##
      |...###.##.
      |
      |Tile 1061:
      |###...##..
      |.#...##...
      |.#.#..#...
      |..##......
      |#......##.
      |#.....#...
      |#.......##
      |#........#
      |#.##.##...
      |##.#.##.#.
      |
      |Tile 3947:
      |.#####.##.
      |..#.#.#...
      |#.......##
      |#.#....#.#
      |..#....##.
      |.##.#...##
      |#....#.##.
      |....####..
      |###....#.#
      |########.#
      |
      |Tile 2213:
      |.###......
      |....#...##
      |#..##.....
      |#..##.#...
      |...#....#.
      |.........#
      |#####....#
      |#......#.#
      |###.......
      |..##.##.#.
      |
      |Tile 3319:
      |..##.###..
      |.#...##...
      |..#.#..#..
      |..#.##....
      |...###....
      |##.#......
      |#.#...####
      |.....#.#.#
      |.#........
      |#.....#.##
      |
      |Tile 1423:
      |#....#..#.
      |..#...#..#
      |.#.#.#.##.
      |#.##..##..
      |#.##......
      |#.#...##.#
      |..#......#
      |#.##...###
      |###.#.#.##
      |.###.....#
      |
      |Tile 1607:
      |#.#...#..#
      |.##....#..
      |.#...#.#.#
      |##.#.....#
      |...###..##
      |#...#..#..
      |..........
      |#..#.....#
      |#..#......
      |.#.##..#.#
      |
      |Tile 3301:
      |...#.#...#
      |.#.#....##
      |.#.####...
      |#..##.#...
      |##..###..#
      |#...#.#.##
      |......#..#
      |#........#
      |#..####.##
      |####..#.##
      |
      |Tile 2917:
      |.##..####.
      |#......#.#
      |##.......#
      |.##..#....
      |......#..#
      |.##..##...
      |#.####.#.#
      |##......##
      |#......##.
      |.....#.##.
      |
      |Tile 3851:
      |#.##.#.#.#
      |###..#...#
      |#.##.....#
      |..##..#...
      |#.##.....#
      |.##...#.#.
      |#..#.##...
      |#...#.....
      |...#.#...#
      |...#..####
      |
      |Tile 1439:
      |..##.##.##
      |.....#...#
      |#..###...#
      |....##....
      |##.#..#...
      |.....#...#
      |##.#.#....
      |...#.#...#
      |..#..###..
      |..#...###.
      |
      |Tile 1493:
      |.#.###....
      |.#.#.#....
      |#.#....#.#
      |#...#..#..
      |#....#..#.
      |..#...#...
      |...#......
      |..........
      |...#.##..#
      |##....#.#.
      |
      |Tile 1889:
      |..###.###.
      |..##.#....
      |#....#...#
      |..#...#..#
      |#.......#.
      |#...#..#.#
      |..#..##...
      |..#......#
      |#....####.
      |##.#.##...
      |
      |Tile 3407:
      |#..#.....#
      |........#.
      |..#.......
      |...#......
      |...#.##.##
      |#..##.....
      |##.##..##.
      |#...#.....
      |#....##...
      |##.##..##.
      |
      |Tile 2699:
      |....##..#.
      |#.........
      |#.#...###.
      |...#..#..#
      |.##..#...#
      |##..#..##.
      |####....##
      |#.#...#..#
      |........##
      |.####.....
      |
      |Tile 2861:
      |.....##...
      |.....#..##
      |...###....
      |..##..#..#
      |..#.#.....
      |#.#.......
      |.....#....
      |.##..#.#..
      |##.#..##..
      |#.####...#
      |
      |Tile 1619:
      |##....#.##
      |#.#...#..#
      |#.#....#..
      |.......###
      |#.#.......
      |......##.#
      |.#....#.#.
      |#....###..
      |#....##.##
      |#.#####...
      |
      |Tile 1531:
      |...#.##...
      |#..#.#....
      |#..#...##.
      |.....#....
      |#........#
      |#.#.......
      |##.##....#
      |......#..#
      |#.#.......
      |..#####...
      |
      |Tile 3169:
      |##.####..#
      |..........
      |#...#..#.#
      |#........#
      |.#...#....
      |#...##....
      |.........#
      |#....#..#.
      |......###.
      |..#.####..
      |
      |Tile 1249:
      |###.#..#.#
      |#..#.###.#
      |#.#.#.....
      |#...#.....
      |###.#.#...
      |.....#.##.
      |#.......##
      |#....#.#..
      |....#..##.
      |..#..##.##
      |
      |Tile 2293:
      |..#...#..#
      |#....#...#
      |#....#..#.
      |..........
      |.......#.#
      |####......
      |.##....#.#
      |####.....#
      |#..#.....#
      |#.#.##....
      |
      |Tile 1723:
      |.#.#..#..#
      |#...#..#.#
      |#.....#...
      |.##..#..#.
      |.........#
      |#.#..####.
      |###..###..
      |###..##.##
      |#.......#.
      |#..#.#.#.#
      |
      |Tile 3191:
      |.#..#####.
      |........#.
      |.#..#...##
      |#..#...#.#
      |.##.##....
      |.......#.#
      |...#...#.#
      |..#.......
      |##.......#
      |...####..#
      |
      |Tile 1663:
      |.####.##..
      |....#...##
      |##..##...#
      |#..#......
      |....##....
      |##.#..##..
      |##..##..#.
      |...#.#.#.#
      |#......#..
      |.##..###.#
      |
      |Tile 2281:
      |####.#..#.
      |#......###
      |.#....###.
      |..........
      |..#.....##
      |.........#
      |..#.#.#.##
      |#...#.....
      |###.#..#..
      |##...#..#.
      |
      |Tile 3677:
      |..####.##.
      |###......#
      |.##....#..
      |#....#...#
      |....#.#.#.
      |#.##.#..#.
      |...##.#...
      |....#.#..#
      |..#.......
      |##..##..#.
      |
      |Tile 1361:
      |.#..###.##
      |.........#
      |#....##...
      |..##...#..
      |#.#.....##
      |#.#####...
      |##.#...#..
      |#..#...###
      |#..#.#...#
      |#####.#.#.
      |
      |Tile 1051:
      |######.##.
      |####..#..#
      |###.##....
      |#.#......#
      |#####..###
      |#.#.#..#.#
      |...#.#...#
      |##.#.....#
      |..#...####
      |....#..###
      |
      |Tile 1831:
      |##.#..####
      |####...##.
      |##...##..#
      |#...#...#.
      |....#.....
      |#...#....#
      |#.....#...
      |#####.....
      |......#...
      |.###.###.#
      |
      |Tile 1861:
      |....###..#
      |.####...#.
      |#..#...###
      |#..#.#....
      |#.##.#....
      |....#.##..
      |.#..#...##
      |.....#....
      |#...#.##.#
      |##.##.#...
      |
      |Tile 1873:
      |.#.###..#.
      |##.......#
      |.####...##
      |##......#.
      |#.........
      |.#........
      |.##.#....#
      |...###.##.
      |..#..##..#
      |.#.###...#
      |
      |Tile 2129:
      |#####..###
      |##..##..#.
      |.#.#......
      |##....#..#
      |#.####....
      |....#....#
      |###..##.##
      |.##.#..#..
      |#.#..##.##
      |...#.##.#.
      |
      |Tile 2333:
      |..#.#...#.
      |#.........
      |#...#....#
      |....######
      |..##...###
      |##....###.
      |...#......
      |##.####..#
      |#####.....
      |.#....#.#.
      |
      |Tile 2851:
      |.###..##..
      |...#..##..
      |#......##.
      |......#.##
      |..........
      |.###....#.
      |#....#.#..
      |#.....#.##
      |..........
      |.#######..
      |
      |Tile 2689:
      |.#######.#
      |.....#....
      |........##
      |###..#.##.
      |#.........
      |#......###
      |.#.###.#..
      |....###...
      |#..#..#...
      |..#...#.#.
      |
      |Tile 1193:
      |.##...#...
      |####......
      |...#.###.#
      |..#..#..#.
      |#.#...##.#
      |.....##.#.
      |#...##.##.
      |#....#..##
      |...##...##
      |#..##.#..#
      |
      |Tile 2663:
      |......#.##
      |##......##
      |.#.....##.
      |.##.##...#
      |#.##..##.#
      |...##..#.#
      |#.........
      |#.#..#...#
      |.......#..
      |.#...#####
      |
      |Tile 1481:
      |.#.#...#.#
      |#..#......
      |.#.......#
      |#.##..#.#.
      |..#..#....
      |##........
      |.#...#####
      |#....#.#.#
      |#....##..#
      |.###.####.
      |
      |Tile 2417:
      |###..####.
      |....#..#.#
      |...#....##
      |#........#
      |#.##...#.#
      |#.#.......
      |.......#.#
      |#......###
      |#..#.##.##
      |#.##.....#
      |
      |Tile 3671:
      |..#####.##
      |....#.....
      |.....#.#..
      |....#...#.
      |..........
      |#.#..##.#.
      |..##.#...#
      |#..#.....#
      |.#......#.
      |...#.###.#
      |
      |Tile 1181:
      |###..#.#.#
      |........#.
      |#........#
      |#..#..#.##
      |#.#..##...
      |..#....#.#
      |..........
      |.####.#.##
      |...#.#....
      |...#....##
      |
      |Tile 1999:
      |#..#..##..
      |...#......
      |##.#.#.#.#
      |#.....#..#
      |...#....#.
      |##..##..##
      |####.#...#
      |....#....#
      |.....#...#
      |#.#.####.#
      |
      |Tile 2207:
      |...##.##..
      |......#.#.
      |###.#.#..#
      |.....##...
      |..##.#.#..
      |#.....##.#
      |.#.#......
      |#...#..#..
      |..#.#####.
      |..###..#..
      |
      |Tile 3359:
      |#.#..#...#
      |#.........
      |####..#.#.
      |#........#
      |....#.#...
      |.#..###...
      |.#....#..#
      |....####.#
      |#.....###.
      |#...##.#.#
      |
      |Tile 1847:
      |##..#.#.##
      |#........#
      |#.#####...
      |..##.##...
      |##..#...##
      |##...#..##
      |.#...#....
      |#.#......#
      |##..##...#
      |..#.#####.
      |""".stripMargin
