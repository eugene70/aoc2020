package aoc2020

import scala.annotation.tailrec

object Day24:
  type InputType = Tiles
  type Tiles = Set[Tile]

  case class Tile(x: Int, y: Int):
    def neighbors(): Seq[Tile] =
      Seq(
        Tile(this.x - 1, this.y - 1),
        Tile(this.x - 1, this.y + 1),
        Tile(this.x, this.y - 2),
        Tile(this.x, this.y + 2),
        Tile(this.x + 1, this.y - 1),
        Tile(this.x + 1, this.y + 1))

  enum Dir:
    case NW, NE, W, E, SW, SE
    def step = this match
      case NW => (-1, -1)
      case NE => (-1, 1)
      case W => (0, -2)
      case E => (0, 2)
      case SW => (1, -1)
      case SE => (1, 1)
  object Dir:
    def parse(str: String) = str match
      case "nw" => NW
      case "ne" => NE
      case "w" => W
      case "e" => E
      case "sw" => SW
      case "se" => SE
    def sumStep(a: (Int, Int), b: (Int, Int)): (Int, Int) =
      (a._1 + b._1, a._2 + b._2)

  @main def runDay24: Unit =
    val testData = time("testReady", () => ready(testInput))
    time("testPart1", () => part1(testData)) // should be 10
    time("testPart2", () => part2(testData)) // sould be 2208

    val data = time("ready", () => ready(input))
    time("part1", () => part1(data))
    time("part2", () => part2(data))

  def ready(input: String): InputType =
    input
      .linesIterator
      .map(toDirs)
      .map(
        _.map(_.step)
         .reduce(Dir.sumStep)
      )
      .toSeq
      .groupBy(a => a)
      .filter(a => a._2.size % 2 == 1)
      .keys
      .map(Tile.apply)
      .toSet

  def toDirs(line: String): Seq[Dir] =
    (' ' +: line).sliding(2).map(str =>
      str match
        case s"s${x}" => str
        case s"n${x}" => str
        case s"${x}s" => ""
        case s"${x}n" => ""
        case _ => str.tail)
      .filter(_.nonEmpty)
      .map(Dir.parse)
      .toSeq

  // 검은 타일의 인접한 6개 타일 중 검은 타일이 0이거나 3 이상일 때
  def flippedToWhite(blackTiles: Tiles): Tiles =
    blackTiles
      .filter(tile => !(1 to 2).contains(tile.neighbors().count(blackTiles.contains)))

  // 흰 타일의 6개 인접 타일 중 검은 타일이 정확히 2 일 때
  def flippedToBlack(blackTiles: Tiles): Tiles =
    val whiteTiles = blackTiles.flatMap(_.neighbors()).filterNot(blackTiles.contains)
    whiteTiles
      .filter(_.neighbors().count(blackTiles.contains) == 2)

  def part1(data: InputType): Long =
      data.size

  def part2(data: InputType): Long =
    @tailrec
    def day(days: Int, blackTiles: Tiles): Tiles =
      if (days <= 0) return blackTiles
      day(days - 1, blackTiles -- flippedToWhite(blackTiles) ++ flippedToBlack(blackTiles))
    day(100, data).size

  val testInput =
    """sesenwnenenewseeswwswswwnenewsewsw
      |neeenesenwnwwswnenewnwwsewnenwseswesw
      |seswneswswsenwwnwse
      |nwnwneseeswswnenewneswwnewseswneseene
      |swweswneswnenwsewnwneneseenw
      |eesenwseswswnenwswnwnwsewwnwsene
      |sewnenenenesenwsewnenwwwse
      |wenwwweseeeweswwwnwwe
      |wsweesenenewnwwnwsenewsenwwsesesenwne
      |neeswseenwwswnwswswnw
      |nenwswwsewswnenenewsenwsenwnesesenew
      |enewnwewneswsewnwswenweswnenwsenwsw
      |sweneswneswneneenwnewenewwneswswnese
      |swwesenesewenwneswnwwneseswwne
      |enesenwswwswneneswsenwnewswseenwsese
      |wnwnesenesenenwwnenwsewesewsesesew
      |nenewswnwewswnenesenwnesewesw
      |eneswnwswnwsenenwnwnwwseeswneewsenese
      |neswnwewnwnwseenwseesewsenwsweewe
      |wseweeenwnesenwwwswnew
      |""".stripMargin

  val input =
    """nenwneswnwnwneswnenwnwsewenwsesw
      |swsesenwswswseseswseswneswseswsweswnese
      |neeswsesweneseeenwnenwnewneseewee
      |wseeeneseneneeeneneenweneewese
      |wwwwsewewsewnwwswwenewwwnww
      |seseneeseeswsenwswsewsewseseseneee
      |enesenesweseeswswswnwwnwne
      |swwwsenwwnwnwnwenwnenwnwsewsewwne
      |wnewwseeeswneneneneenweeesw
      |swwwswnwnwewwseswwesww
      |nwneswswswnweswweneeeneewwswswswne
      |senwseswsewseseseesesenwesesenweseesw
      |senesewneenwnwwesewnwesesesenewne
      |swwnwnwwwnwnwwewwnenwnwswsenwwnenw
      |swnwneseewnwnwnenwenewnwnw
      |swesesweswseswswnwnwsewnwseswseeswsw
      |nwnwnwsenwnwnwnwnwsenwwenwnwsewnwwneswnw
      |wwwewnwnwswwwneseeeswnwnww
      |swswswswseswneswseseneneswneseswswnewsw
      |swneseswswseseseswswswseswswse
      |wnewwwswnwnwwwwnwwsenww
      |senwsenewseneswwnene
      |nwseeswsesenesenwseseseseswsesenwseseswsw
      |wwswwswsewnewewwwwwwwwwnenesw
      |wwnwewenewweswnewswse
      |swwsenwwnwwwnwwseenwnwwnwnwnwenww
      |wwnenewswwewwswwwwsewwnwww
      |swenwnweseeseeeewe
      |nwewseeeesesese
      |nwsewesweesenwnwswseesewewswswse
      |wewsewwswwwnwewwwswnewwww
      |swswwneswwswswwwswenesene
      |seseseswswseswseswwseswswnesesw
      |newnwwwnwwwsewwnw
      |eswseenwseeeeeeswsenwneeeeee
      |swswsweswneenwneweneneneeeneesesw
      |swnwswesweneswswneseswswwswnw
      |eeeweeeesweneweneeeeeese
      |esesesesewnwswseswnesesesenweseseseew
      |nenwsenwnwnwsenenenenenwnewnenwnenesese
      |neenwneneneenwneneswnenwewneneeswswnenw
      |swseeswswswswenwswswswnwswswswsesw
      |nwnwswnwwenwnweneenwswnwsenwswnwsese
      |wenwweswseswswswswswwnwswswneenww
      |wswesenenwnwwenwwnwswnwnwnwnw
      |wnwnwnwsenwnwwnwnwnwwnwnw
      |ewesweeeeenenenwneswswseeeweee
      |nwenweeeeweeseeenwsweseeesese
      |senewneneneneswnenwseenwsenesewnwnenwse
      |senwseewewseenwseseseseswese
      |eeeeneswsenweeee
      |ewnenwnweswnwwswswnwnwnenesesewese
      |newnwswwnwenwwnwnwswwswnwwwwenw
      |nwseewwnenwnwnwnwnwnwnwsenwnwnwwenwnw
      |ewnenenwnenenenenwesewswwnwnenenee
      |seseswsesesenesewsesesenwseseewnwsese
      |nwswseseseeseneswnwwseeeesenweswwe
      |enwnwnesenwnenwnwnenenwwnenwsenwnwsenw
      |wewnweseeswneenenwseeeeeseee
      |sesesesesewseswseseseesesese
      |neeeeeneeswneseenwnenenwneswenwe
      |sewswswesweswsewseseswnwneeswnwenw
      |wnenenewswseneeenewseeenw
      |neswnwnenwwewnwnwnwnwnwseweswnwese
      |swnwswsewswswnenwnewsewwwwneseswse
      |wwswsweswwswswswswswswsw
      |esenewneneseweeenenew
      |nwswewnwsenwnwenwnwwsewwnesenwnwse
      |wenwweeseseswewenenweeeesew
      |sesenwswnwnwnenenwnwnewnesenewnenenenww
      |sesenesweswwesenwswnw
      |nwsenwswnenwnwnwsenewneswsenwwsenwnwnwne
      |nenenwswnwnwnenenwnwnwnwnwsenwneewnesw
      |nwneneeseeswsenwesewseswwseswwnesene
      |nwnwnwseneenwnwnwnwneswswnwsweenwnwnww
      |swnenesewnwseneswnwswseeseswseeenese
      |newneneneneneeswne
      |nenwnenwnewnenenenenwneswsesenwnenwnene
      |swwnwnwewnwnenwnwnwnw
      |wseenwswsenenewnwswswnwnenwnwswenwsenw
      |enenewnewnwnwneeneneseswswnwseneew
      |neneeneeneneswnenewnenwe
      |senewnwswsenwnenwsenwsewnwwneenwnwsw
      |nenwneeneeeneneeese
      |neswswswesewwswwswswsenesewswneswnew
      |wwsewwnwwwwwwnwnw
      |neneeeeeeeneswnewneneeseneneew
      |swswswswwwswswwnesw
      |seeneneewweenese
      |eswnwsweswswwnwseswswswneswseeswswne
      |wwnwswwwnenwnwwnww
      |nwseeswnwneeseswseseweseneesweseee
      |eseseseseeseeseseseenwsese
      |swesenwswseswswsewsesenesese
      |enesewnwswswnenenenenwneneneswsenwnenw
      |wnwwsesewwnewswwwwnwnwwwenwe
      |eswswswseswwnwnwswsweseswenwsw
      |enwswseswwwswnwswneswswwwseswswswsw
      |wsenwswswswswsweeneswwwwswwsw
      |esenwewwwwwwwwwwewwwww
      |neneneseenwsenwsenwswwwseenewnenesese
      |nwweswwsenwnwesweweswnwwenwenww
      |eseswsesesesenesesesesewewsenwnww
      |newesenwnenenenenwneneneswnesenenenewne
      |ewnwswnenwnwsenwsenwsenenwnesenwneswnwne
      |nwwswnwsewnenesesweenwnwnwewnenwnw
      |swnweswswwswswswswswnwseswswswsw
      |eewwswnewswewenwnwseneseenwsenwe
      |nenwswnwenwnenesewnenwseneswseswnwnenene
      |neneswnwwnwewsewenwsesw
      |senwseseseseseseswsenwsesesesesesesenesenw
      |enewsewwnwnwseswnenwswnwwnwwnenwnw
      |wswwwwswswnweswswwswwwwwwenwe
      |swswneswseseseseswswsw
      |wwswsenewwwwwswwswneww
      |wseeeewneseese
      |nesewwseseseswseseesw
      |nwenwnwswnwnwswenwswnwenwnwsenwnenenwnwnw
      |seswswswseswseneswseswswswneneseenesenwse
      |wswswwswwswswsweswnwswsweswenwewsw
      |sewsenwsewseneeswseeswse
      |eewweewneeneeeseneweeswnene
      |weeeenwesenweeseseewnewsenwse
      |nenenewneswneswneneneeneneneeeseneswe
      |wnwneneseenwewseswswswswneswwswnene
      |nweeswwwseseeseweswswnwsenwswswse
      |eneweenenwseweesenww
      |swnenewnweneseewnenwnesenenwenwnenwnesw
      |seseswseseswesenesesewswsesw
      |neeseweneeesewseeeneenwweeseee
      |newseneswnenwnwnesenenwnwnwenwwnwnwnw
      |enwnwenwnwwnwnwnwnwnenwnenwswswneese
      |ewwwwswwwswswwswnewswnew
      |sesesesenwseseseseseesese
      |nwnwwsewnwnwnenwwenwswseseenwnwswne
      |wswneneswswseswseswewwsesesweseneswnwnw
      |wnwsewwwwnwswwwwwwwenwew
      |nwnwnwnwnwnenwnwnwsww
      |wswswswnwswwswnwsenwseneswsweswswseswsw
      |nesenwneseswseswwnwneseswsesesenwswesese
      |nwnwnenewsenwneseenwnenwnwnwneseneswnwswne
      |nenesesenenenwnwswneeewnenenw
      |swnwswseswswwnwwwwswswnenewsewswswese
      |swenenwnwnwnenenwswnenwnenesweenenwnw
      |eeswenewenwneswneneeneenenenee
      |swswsweswswneswswswnenwswsweswnwsenwne
      |eeeesesewesenwesw
      |wwswswseneswsesenwseseneseeswseenwnwsw
      |swwwwewswwesw
      |wswwswwnewwseswswswswewswnwwnesw
      |eswsenwseswneewenweeewwwneene
      |wwewweswswwwswwewswwnwwwnw
      |swwneeenwnenwnwwswnenwnwnwweenwne
      |eneseneneswneneeneseneenweweswwnw
      |nwesesesesweeseneseesesesesesenwsenw
      |wwwwwwwwneswwseswwnesewwswesw
      |wnwesenwnwsenwwwnwwnwnwwwnewswwnw
      |eneewneeeneneneneneeenewese
      |sesesenesewsesesesesesenwsesesesenw
      |seswsenenwwsewseneneseeseswswswsesesese
      |swnenwswnwnewnwswswwnweewsesesewwne
      |ewneswswneswnwnwwsewneneseswseswsesee
      |wswswwswwnewwswwswswwewswsewe
      |swwnwnenenesewnenwwnwneneeweeswsene
      |eseswseseeseeeeneeswneneneweswee
      |eseseseenenesesesesewneseseseseseww
      |enesenenenwneneneneenenene
      |wswwswwwnewnwwsenewnwwnewseew
      |nenenenwneneeenwnwwswswnenenwenwnesw
      |nenenwnewswneneneneseneenenenenenesene
      |nenenwnwnwnwsenwnwnwnw
      |swwnweenwneneneeswnenenenenewswsenenenw
      |eswnenwswswnwwewneseesweseneenww
      |nenenenenwwnenwneneenenenw
      |sesewswneswswswseswneswswsenwswseswswse
      |esesweeeeneneesweeeenwwnwneee
      |wwwwnwwnwwsenenwwwwwnwwswseew
      |nweseneswnenenenenenenwnenwnenwne
      |wnwnwsenenewnwsewsenenenwewsenwwee
      |wnwseswsesesweesesenwseswswse
      |nwnenwwnwwneneneeswseneneeswnwnesene
      |neswsesesesewsesesenwswseesesesesewnesese
      |nwwwswwwewwww
      |swsweseseseseneswnwnesewseseswseseswsw
      |esesenwseseswwseseesenwnenesesesewsew
      |eseswswnwswnwsesewesesweswsw
      |wwwnwwwenwsenwnwnwwnw
      |wnwnwnwnwnwnwwnwnwnwnwnenweesenwwswnwsw
      |swswsesweenwnwwnene
      |swswswwneneneswwsesenwwswnwnwwwsee
      |nenwsenwenwenwsenenwnwnwnwneswnenwnww
      |swewwnenweswnwswwnwnwwwnwnwwwww
      |newneneenenenenenenenenee
      |swewnwswswneeswseseswnesenwenwwswsesw
      |wnewwswenwwwwswnwwwnewnwnwsww
      |nesenwswsweneseswneesesewswnenwsesesese
      |sewswswswwwnewswswwnwwnewwwswwsee
      |seeeewswneseewsesenenweseesesese
      |nenwneswnenenwnwnwwnwenwenenenwnwnwsw
      |enenwsenwwnwneswseseseenewseswsenww
      |newswswwswwswswwswnwwwswswneesesw
      |nwswswnwnwswseswswwneseenwwswe
      |nwwwswseneneneneneenwneneneneneenenene
      |senwsenwwwwwsewwwwneswnenwwww
      |swswwswsenwswseesesewnenenenewswsweswnw
      |swweneenwwsenwneneeneseneeene
      |nwnwnenwswenwnwwnwewnwnwsenwnenwnewnwe
      |eenenwneswnenenese
      |senwnwneseseeewswsesewsewenesesese
      |neswwwewwnenwwswwseswnw
      |wseeeneswseseeseseseseneneseseswsese
      |senenewesenwesenwswwwswnwsesenwenw
      |nweneneeeseeeeeseenwweeeswne
      |wswswseseswswswwnwnenwsweswswswswwsw
      |nenwswnwenwneswseesenenenewneee
      |nwnwnwnwnwnwenwswnwnwnwwnw
      |seseseseseneswseseseswswsw
      |eswswnewwswsenweswwwnwswswesenww
      |nenenenwwnwneenenesesenenewne
      |neeeswneswnweeeeeneneeswneenenew
      |weswswenwseswswswswsenwswsw
      |nwnwnwnwnwwenewwneewseseswnww
      |nwwsenwwswswneeneswswswsesweswswnesenee
      |wswwneswswswweseswnewnewnenesesesenenw
      |sweseseswswswsewsw
      |eeenewseneeweseenenewneseeneenee
      |seseenwnwseseseeseseeeneweseseseew
      |wwewnwnwwwsweew
      |sewnwnenwnwesenwwnenwnwwnwneswwwsew
      |wswwwswwsenwnwwwneswewswenwswswwsw
      |nwnenenenenewenenenenenwnene
      |sewnesewsewsewnenwwwwnewnwnwww
      |nwsewneeswwsewneswnwweenwnenw
      |eseeseseneseseweesesenwsesese
      |eneenenewenwswnwseswsenweeeeee
      |sesenwweesesesesesesenesewsesesesese
      |eneenwneeeewseenewewnweswseswe
      |swwswswswswswweswnweswswswswsesweswswnw
      |eewseeeenweeeenweswseseesee
      |nwnwneeswnwnwwnwwswswneswswnwnwenwswene
      |eenwnwswesenwsweswsesenweseenwee
      |wwnwwwewwnwwnenwwwsw
      |swnwnwneswenwenenenewenwswseswewse
      |nenwnwnenwnwnwnwnwswnwnwnwnw
      |nweswneeeswnweswswenwsweswneeenenwne
      |nwnenwneesewnewnwesenwnwnenwnewnwnwnww
      |ewwwwswswwenwwswswswwsesw
      |nwesewnenwesewseswnenweswseewseseee
      |nwswwsweswswseswwswswswnwswnwswswwe
      |wenwseewseeseneeseswswseseenwsene
      |seswswsesesesesesesenw
      |enenewenenenwnenwnenenwnenwswseswne
      |swwwsesewwwnwsewwsewnenwnwwwwww
      |wsenwwnenwnwsenwseseseseswseswneenesese
      |sweeeenwsenwnesenenwswswewwnwesww
      |swseswswswswsenwswswsw
      |nwswnwswnwnenwnwnwenwnwnwnenwnewnwnwnwsew
      |weeeeseweseweneenwsweeseenew
      |nwswswseswswswseswswswsw
      |eswsenwswsesenwswneneeenwneseswwswse
      |seseseseeseseseeenewsesese
      |swnenesweweenwneeeneneseeeeee
      |wwwwnwwwsewwnesewwnewse
      |neeenenwnenwswswnwseneeeeesweee
      |neenenenenenenwenewnenwswneswenwnene
      |wseeenesesenwneesenweeseswswswswse
      |esenwweswwewnenwewseenwsenwsee
      |newneeseeseeswewseeesesenweeesee
      |esewnwsenenewswnwwwsenwseswnwewnww
      |neneneeneenwewweneswseneneneneese
      |neewswnwweseswneesweswnwenwenenw
      |wwnwwnwwwwwwwsewewww
      |seseseneseswsesewseswseseswnwsee
      |esweeswseweseseeeenwswenwneene
      |sesenwwseseneseswswnwswsewseseseseseene
      |eeesenwsweeenwneeswswneesenewne
      |nenwnwnenenwnenenwwewewnwesenwswnw
      |nwnwnwwnwseenwenwnwnenwnwnwwnwnwwne
      |wneseeeenenewnwnwwneneesweswnene
      |wwnwwwnwsenwwewwnwsenwnwwwnew
      |nwnwnwnwnwwnwnwwswsesenewenwnwnwnwww
      |eswwsewnwwsewwnwwwwewwwenwsw
      |nesweswnewswnwswswewwnwsenwsenwwnwse
      |nwnwwwnwwweweswsenwwsewwneeww
      |neeenwswswnwswsweneneenwneeeneeneee
      |nwnwnesenwsewnwnenenwsewnwnee
      |eeenwesenwsesenweesweseseeswesee
      |eewwwwswwswswwswnewwnwnwesww
      |sewnenesenwnwswswnenwseneswwnenwnenwesw
      |ewswnwnwnwenwnwnwnwnwwswnwwenwenw
      |nwnwnwnwsenwnwnwnwnwnwnwnwwnw
      |newwwswswswswseswswswswswesw
      |swswswwwswwesenwswwneswswswwswsw
      |neeeenenweeeneneesw
      |seesesenwenwsewseenwsewswnweneew
      |swnesenesenesewsweneeswnwsesenwwese
      |eswnwswwswsenesenew
      |swneswwenwnenwsenenewwswesenenenenw
      |wwwwwwwwnwswneeseswneesesw
      |sesenwnwwnwwnesenwnwnwwnwswenwnwwene
      |swswswwsesweseswswwswnwswswsesesenee
      |nwnwswwnenenwnwwnwwnweswnwsenwnwnwnwnw
      |""".stripMargin
