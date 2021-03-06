package aoc2020

object Day19:
  case class InputType(val rules: Map[String, Any], val patterns: Seq[String])

  object Rules
  class Rules

  @main def runDay19: Unit =
    val testData = time("testReady", () => ready(testInput))
    time("tesPart1", () => part1(testData))
    time("testPart2", () => part2(testData))

//    val data = time("ready", () => ready(input))
//    time("part1", () => part1(data))
//    time("part2", () => part2(data))

  private def ready(input: String): InputType =
    val in = input.split("""\n\s*\n""")
    val rules =
      in(0).linesIterator
        .map(_ match
          case s"$id: $rule" => (
              id, rule match
                case s""""$literal"""" => literal
                case links => links.split(" \\| ").map(_.split(" ").map(_.toInt).toSeq).toSeq
          )
        )
        .toMap
    InputType(rules, in(1).linesIterator.toSeq)

  private def part1(data: InputType): Long = {
    //data.patterns.count(matches(data.rules, _))
    println(fetch(data.rules, "0"))
    0
  }

  def matches(rules: Map[String, Any], pattern: String) =
    0

  def fetch(rules: Map[String, Any], key: String): String =
    rules.getOrElse(key, "") match {
      case x: String => x
      case s: Seq[Seq[Int]] => s.map(sx => sx.map(sxx => fetch(rules, sxx.toString))).toString
    }

  private def part2(data: InputType): Long =
    0

  private val testInput =
    """0: 4 1 5
      |1: 2 3 | 3 2
      |2: 4 4 | 5 5
      |3: 4 5 | 5 4
      |4: "a"
      |5: "b"
      |
      |ababbb
      |bababa
      |abbbab
      |aaabbb
      |aaaabbb
      |""".stripMargin

  private val input =
    """102: 100 47 | 76 84
      |23: 60 47 | 73 84
      |132: 17 47 | 81 84
      |108: 55 100
      |18: 116 47 | 26 84
      |103: 84 115 | 47 81
      |65: 84 113 | 47 50
      |128: 107 47 | 125 84
      |14: 84 100 | 47 107
      |118: 47 17 | 84 57
      |2: 47 100 | 84 40
      |28: 63 84 | 74 47
      |22: 102 84 | 123 47
      |123: 84 74
      |19: 3 47 | 13 84
      |24: 74 47 | 81 84
      |115: 55 55
      |90: 92 47 | 44 84
      |48: 84 94 | 47 96
      |109: 17 84 | 100 47
      |92: 84 75 | 47 108
      |66: 38 47 | 125 84
      |83: 66 47 | 108 84
      |31: 121 84 | 77 47
      |29: 47 61 | 84 111
      |45: 47 47 | 47 84
      |59: 47 49 | 84 43
      |37: 47 30 | 84 95
      |36: 107 84 | 125 47
      |82: 74 84 | 38 47
      |61: 84 10 | 47 110
      |79: 47 28 | 84 109
      |33: 101 47 | 133 84
      |12: 45 47 | 63 84
      |91: 122 84 | 93 47
      |122: 65 47 | 52 84
      |21: 57 84 | 115 47
      |8: 42
      |67: 102 47 | 64 84
      |39: 113 84 | 81 47
      |41: 84 124 | 47 10
      |50: 47 47 | 84 84
      |17: 47 84 | 84 84
      |120: 98 84 | 78 47
      |113: 55 47 | 47 84
      |20: 84 128 | 47 104
      |7: 84 1 | 47 20
      |51: 84 113 | 47 81
      |56: 84 83 | 47 69
      |131: 84 127 | 47 97
      |0: 8 11
      |5: 47 63 | 84 125
      |94: 15 84 | 127 47
      |121: 99 47 | 27 84
      |119: 47 115 | 84 57
      |129: 47 80 | 84 131
      |15: 47 100 | 84 45
      |35: 84 50 | 47 76
      |95: 47 115 | 84 107
      |68: 127 84 | 51 47
      |124: 84 107
      |75: 50 55
      |57: 47 84 | 84 55
      |13: 47 33 | 84 129
      |53: 106 47 | 59 84
      |106: 16 84 | 118 47
      |89: 84 125 | 47 45
      |104: 45 84 | 76 47
      |99: 47 56 | 84 7
      |78: 84 74 | 47 81
      |64: 17 47 | 115 84
      |32: 50 84 | 40 47
      |1: 47 24 | 84 72
      |47: "a"
      |80: 114 47 | 109 84
      |88: 47 119 | 84 132
      |105: 47 125 | 84 100
      |6: 68 84 | 67 47
      |110: 76 84 | 63 47
      |38: 84 84 | 84 47
      |49: 47 63 | 84 76
      |26: 55 107
      |81: 47 84
      |74: 84 47
      |96: 84 89 | 47 117
      |77: 47 86 | 84 71
      |135: 32 84 | 2 47
      |133: 47 15 | 84 128
      |42: 19 84 | 62 47
      |30: 47 100 | 84 74
      |27: 6 47 | 91 84
      |63: 84 55 | 47 47
      |62: 84 87 | 47 23
      |76: 84 84
      |4: 84 135 | 47 54
      |60: 41 47 | 37 84
      |100: 47 47 | 84 47
      |85: 47 112 | 84 18
      |116: 125 84 | 63 47
      |134: 57 47 | 115 84
      |34: 52 47 | 25 84
      |40: 47 47
      |111: 58 84 | 126 47
      |3: 29 47 | 130 84
      |114: 17 84 | 107 47
      |52: 47 45 | 84 74
      |10: 47 100 | 84 81
      |98: 47 76 | 84 100
      |112: 84 82 | 47 103
      |72: 40 47 | 45 84
      |126: 50 84 | 113 47
      |107: 84 47 | 47 84
      |11: 42 31
      |55: 84 | 47
      |54: 12 84 | 5 47
      |130: 34 47 | 70 84
      |84: "b"
      |127: 81 47 | 17 84
      |87: 84 53 | 47 9
      |101: 105 84 | 14 47
      |9: 88 84 | 120 47
      |73: 47 79 | 84 22
      |97: 74 84 | 50 47
      |117: 74 47
      |70: 47 134 | 84 46
      |58: 47 50 | 84 115
      |125: 47 47 | 55 84
      |46: 47 81 | 84 17
      |86: 84 90 | 47 85
      |25: 38 84 | 63 47
      |69: 39 47 | 78 84
      |43: 47 100 | 84 125
      |93: 84 66 | 47 35
      |44: 47 21 | 84 36
      |16: 57 47 | 107 84
      |71: 48 84 | 4 47
      |
      |babaaabbbababababbbbabbaabbaabaa
      |babaaaabaaaaababbbbaaaaa
      |abbabaabbaaabababaabbbbabbbbbaabbbbabababaaaabbbbababbbb
      |bbbabababbabbbabbbabbbbb
      |babaabbbababbbabaabaaaaa
      |abbabaabbaababbabababbbababbbabbabbabbbabbaabbbb
      |aaaaabaabbbbbaabbbbbbbbbbaaabbaaabbaabab
      |baababbabbbabbaaaaaababbaabbababbbabbbbb
      |aaabaaaabbabbbabbabaabbbabaabbbbaaaaabaaaabaabaaababaabbabbaaaabbbbaabbabbabaaaaaaaabaab
      |ababaaabababbbababaaabab
      |bbbbabbbbbaabaaaaaabbbbbbabbbabaabbaaababbbaaaaababaabaabaabababbabbbabbabaaabbaaabbaaaa
      |babaabaaababaaabaaaababa
      |aabaaabbaabbbabbabaabaab
      |bbbaabaaaabaaabbbaaabbbabbbaaabb
      |aaaaaaabaaababbbbbbbbbbbbaaabaabbaaabbab
      |bbabbabbaaaaabababaababa
      |babbababbbabbaababaababbaaaaabaaabbbabba
      |bbbbbbbababbaaaaabaaabbaabbaaaab
      |bbbbbaaabbbbbaaaaaabaaaabbbaaabb
      |babbababaababbbbbbbaaabb
      |abaabbaabbababaabbababba
      |abaaabbaaabaabbbabababba
      |babaaaabaaaababbabbbbaaa
      |bbbbbbbabaababbaaaaababbabbaabbabbbaabaababaabbbbbbaabab
      |aaabbabbbaabababbaabaaba
      |bbbababbabaabbabbbabaaaa
      |baabbabababaaaabbbbbbaab
      |bbabbbbabaababbabbbaabab
      |abaabbabbbabbabbabbbabba
      |bbbbbbbaaababaaabbaabbbabbaaabbbbabababb
      |aabbaaabbabbabaaaabbabaababbbaababaabababaaabbaa
      |aaabaababbbabbbbaaababbbbaaabbbb
      |aaaaaaaaaaabaaaababbabbb
      |bbbabbbbaabaababaabbabab
      |baaabaabbaaaabbbbaaaaaaa
      |bbbbbbbaabbabbaabaabaaaa
      |aaaaabaabbabbaaaaaaaaabb
      |aabaaabbaabbabaaabbabaabbaababaaabbbaaaaababbaabaaabbaaabbaaabaa
      |bababbbaaabaababbbababaabbaabaabbaaaabbabbaabababbbabbbababbabbb
      |abbbaababbaabaaaaababbbaaaabbbaa
      |aababbbaaaabaabbbbabaaaa
      |aaaaabbaaabaaabbbababaaa
      |bbbababaaabaaaabbabbabaabbababababababbbabbaaaaabbababbabbbbabbbabbbaabb
      |bbbabababbbabababababbbb
      |bbaaababbbabbbbaabbbbbaa
      |bbababaabaaaabbaabbbbaaa
      |bbbbabbababbbbaabbaaaababbababbaabaaabababbabbabbbbbbaababbbbabbbbaaaaabbbaabbba
      |abbabbabbbbbbaabaaaaabababaaaaabaaabbaaaabababba
      |bbbabaabaababaaabaabbaaa
      |bbababbbbaaababaaaabbabbaaabbbaa
      |aabbbbababaabbaabababbaa
      |aaabbabbbaabbbabbbbbbbbabaaaabbaaabababaabaaabab
      |aaaaaaababbabbabbabaaaaa
      |aababaaaaaababbbabbabbabaaababbbbbabbababababaaaabbabbbb
      |babaabbbbaaabaaabbbabbab
      |bbbbabbbbaabbbabbbaabbbaabbbbbaabbbbaaabbbbaabbbbaababbaaaabaaaabbabbabbabbaabbb
      |aaabbbabbaaabaabbaaaaabaabababab
      |abbababbabbbbbabbaabbbabaaababbbbbbbabaababbbbbabababbbb
      |bbaabbbaaaaababaaabaaaabbbbbbaabbaaaabbaabaaabaabaaaabbabbabbbaabbaaaaaa
      |abbabbaaaabaaaabbbbbbaabbabaababbbbabbba
      |bbaaaabaabbababbbbaabbaa
      |aabbaabaabbaaabbaaabaaaaaaababbabababbaa
      |baaaabbaaaaaabbbbabbababbaabbbbabbabbabbaabbaabb
      |babbaaaabbaabaaabbbbaaaabbbabbaaaaaaaabb
      |aaaaababaaabbabbababbabb
      |abbabaaaaaabaababbabaaab
      |aaabaababbaabbbaababbabb
      |babbabaabaababbabbabaabb
      |abbabbaabbabbbabababaaaa
      |bbabbbaaaababaaaaaabbbaa
      |bbbbbbbaaaaaabbabbbaabaaaaabbaaa
      |bbbaabaababaaabbaababbabaababaabbbababba
      |bbaaabbaabaabbbbbbbaababbbaabaabaaabaababaaaaabbbaaabaababaaaaabaabbbbaabaabbbaa
      |baabbababbaaaaababaabbabaabbbaabaabbaabbabbbaaababbaaaab
      |bbabaaaaaaaabababaabbababbaabbbbabbbaaababbaababaabaaaaaaaabaabb
      |baaaaaabbaababbaababaaaa
      |bbbabbbbbbaabaabbaaabbab
      |babbbabababaaaabaabbaaabbbabaabababbbaaabaababbbbbaababbbabbbabb
      |aabaaababababababaabbbababbababbbabbaabb
      |bbaabaaabbaabababbaababaabbabbba
      |bbbabaaaababaabbaabbabbbbabbaabaabbabaabbaabaabaabbaabbabaaabaaa
      |abbaaabbbbbbbbbbaaabbaab
      |baabbbbbaabbaabbaabbababbaabaaba
      |baaaabaaabbbbaabaabbabbbbaababbb
      |abbbbbbaabbbbbbbbbbbbaaaaaaabaaa
      |abbbbbbbbbabbbaabbbaaabaaabababa
      |aababbbaabbabaabbabaaaba
      |bbabbaabbabbabaaababbbba
      |aaabbbabbaabaabbbbbabbbbaaaaabbbbabbbabababbbaaa
      |aaabaabbaabaababaabababb
      |bbababaaabbbaabaabbababa
      |baabbbaaaaababababaabbaababbabbb
      |babbbaabbaaaabbaababbabb
      |bbababbbababbbaabbabbbabaabababaaabaaaaa
      |babbbbbbbbabbbababbbbaab
      |abaabbbbbabbbbbbaaabbaba
      |bbabbbaabbbbbaaabbbaaaaa
      |abbbbbbbbaabbbaaabbbbbbabaaaabaaaaaabbbb
      |aabaaabababbbbbbaaabbaab
      |bbbabababbbbbbabbbbbaabaabababbaaabbbbbb
      |aababbbaaaaaaaaaabbaaaaa
      |ababaababaababbbabbbbabaaabbabaaabbabbaabbaaaaaababaabba
      |aabbaaabaabbbaabaabaabbbaaabaaab
      |aabbaaabaaaaabaaaaabbbabaaabaaaababaaaaaabbbbbaaaababbaa
      |babbbaabaabaaabababbabaababbbaabbabaabbaabababbababaaaaa
      |aabbbbabababbbaaaaabaabbbaabbabaaaabababaabaaaabaaaababaabbbabbaabaaabbb
      |bbbbbbababbbabaababbabaabababababaababbabbbbbbbabbaaabbb
      |bbabababaaababbabbababba
      |abbbbbbabbbbbbaaabbbaabbbbabaabaaaaaaabb
      |aaaaabbbbaaaabbabbbabbab
      |aabbaaabbbbaaaabbabababbbabbaabbbbababbbbaaabbbababaabbbaabaaaaababbbaaa
      |ababbbabbbabbbaaababbbbbaabbabbaababbaaa
      |abbabbababbababbbbbbabbb
      |aabbaabaabbabbabbaabbaab
      |baabbbaaabbababbabbaaaba
      |baaabbbaaabbbabbbaaaaaaa
      |bbaaababaaaaabaabaaaabba
      |aabaaabbbbabbaabaaababaabbbbaabb
      |bbbbabbaaaabbabbaabbabab
      |aabaaaabbaaaaabababbabba
      |bababbbabaabaabbaaaabaab
      |ababaaabbbaabaabbbaabaaababaabbbabaabbaaaabbbaaabbbaabab
      |aababaaabbbbaaaaaaabbbbb
      |abaabbaabbbbbaaaaaaabaaa
      |babbbabababbaaaabaaaabab
      |abbbbbabbabbaaaababbaabb
      |bbaaaaabbbbababaaaaabaab
      |bbaababaaaabaabbabaabaab
      |bbababbaabbbbabbbaaaaabbabababba
      |bbabbaabbbbbabbaabababbbaabbbbbabaabbaaabbbabbbabaabbaab
      |abbabbaabbaaaaabbbabbbaabbbabaaabbaaabbbaaabaaab
      |aaababbababbabaabaabbbababbabbabbbaababaabbbabbaabaababa
      |baabbaabbbabbababbaaaaaabaaabbab
      |aabbabaabbbbbbbbbaabaaababaaaaaa
      |abbaabbabaaaaabaabbbabaabaaabaabaabaabbbaabaabaaaabaaaaa
      |baaaabbabaababbababbbababababbbb
      |bbabbaabbaaabbbabaaabaababbbbbbabbaaabaa
      |baabbbbaabbbbbbabaabbaab
      |baaaabbaabbababbaababbababaaaabbbbbbbabababaaaba
      |baabbbbbaabaabbbbbabbaaaabbaabbaabbbbbabbbbbbaba
      |babaabbbbbaabaaaabaababbabbbbabbbbbaaaaa
      |baabbababbaababaabbbaaab
      |bbaaaabbbaaaaaabbbabababbaabbabbbaaabbaaababaabb
      |abbbbbbaaababaaababbaaba
      |baabbabababababaaaaaababbababbbabaabbbaaabaaabab
      |aaaaababaabaabababbbbbbbabbbaaba
      |bababaaabbbaabbabaaaaabb
      |aabbaababbabbbbabbbaaaab
      |bbbbbbbbaabbbabbbabbbbba
      |aabaababbaabaabbbbaaaaababaaaaabbaaaaaaa
      |aaaaaaaaaaaaabababbbbbbabbbaabab
      |bbbaabaaababbbaaaabaaababbababbbabaaabbaabaaaaaa
      |ababbabbbbbbaabbaaabbaaabbaaabbaaaabbbba
      |baabababbbbbabaaababbbaabbaaababbbbaaaababbabbbb
      |bbaabbbaabaaabaaababaabb
      |bbabbbaaabbaabbbaaaabbaa
      |bbbabaabaaaaabaaaababaaaabaaabaaaaaaabbaabbaabbabbbbbaba
      |baaaabbaabbbaabaabbabbaabbbaaabaaabbbbbaaaaabaab
      |bbabbabbbbbbbaabbbababbbbbaabaaaaaabbaaa
      |abbbbbbbabaaabbbabbaaaabaaabababaabaabababbbaaaaabaaaababbababbabbbbabbb
      |baabbabaababbbaabbababba
      |bbaabbbaaaabbbbaaabaabaa
      |baabbabaabbbbbabaaaaabaababbbbaaabaaaaaa
      |baaabaaabbbababbababaaabaababbaa
      |aaababbabbaaaaabbaabababbbabaaba
      |aabbbaabaabbaaababbbaabb
      |bbbbbbbbbaabababaabaabaa
      |ababbaaaaabbbbbaaaabbaabbaaabaaabaabbaabbabbbababbbbabbabaaaaaababaaaaba
      |baaaaabaaaaaababaabbaaaa
      |aaaaaaaabbabababaaaabbaa
      |aababaaaababbbaabbbaaaaa
      |aaaaaaabbbaaabababbbaaab
      |aaababaaaabbbbababbaaaab
      |aaababaabaaaabbaabbaabab
      |bbaabaaaaaabaabbbabaaabbababaaba
      |ababaaabbaaaabbabbaabaaaabbababbaababaaababaabbbbabbabbb
      |aaaaaaabaaababbbbbaaaabbbbabaaaabaaabbabbaabbbab
      |abbabaaaabaaabbabbabbaaaababbbbbbbbaaaaa
      |abaaaaababbbbbababbbbaba
      |abaabaaaaabbbbaaaaaabaab
      |babababaaababaaaabaababa
      |bbbaaaaabbbbbbaaaabbaabb
      |aababaaaaaaaabaaabbbbaab
      |ababaaabababbbaaaaaabbab
      |baabbbabbbaabbbaabbbaabb
      |baabbbbaabbbaababbababba
      |bbbbabaaaaaababbbaababaa
      |aaaaabbabbbaabbaabaabbbaaabababababbbbba
      |baabaaabbbaabaabababaabb
      |aaabaaaaabbabbaabaabaaaa
      |aaabaabaabbabbabbabaabab
      |babaabbbbaabbbbbabaaabbabbaabaaabbababbababbbaaaababaabb
      |baaabaaababbabababbaaabbbaabaaabbaabaaaa
      |baaabaabbbababbbabbbbbabbabbaaab
      |abbabaaaaabbbbbababbaaaaaaaaababbabaababaabbbbbb
      |abbbbbbaabbbbaaababaabaaabbbbabbabbbbababbababbababaaaabbbaaaaba
      |aaabaabbabbbabaaabbaaaaa
      |bbbbaaabbbbabaaabaaabbbb
      |babbbaabbaabbbbbaaaabaaa
      |aaaaabaabaabaaabaabbaababbaababb
      |baababbbababbabbbbbbbaabaaaaabbababbbbbabaaaababbbbbbbbbbbaaaaabbabaaaabbababbab
      |baabaabababaababaaabbaaa
      |aababbabababaaababbabaaabbababbaabaababa
      |bbabbbabbabbbaababababba
      |abbaabbbaababaaaabbbabab
      |aaababbbabaabbbbbbbbabbb
      |aabbbbabbaaabaaaabbbaabb
      |abbbbbaabbbaabababaabaabababbaba
      |aaababaabbbbaaaababaaabbbaabbabbaaaabbaaababbaaa
      |bbaaaabaabbabaaaaabbaaabbbbabbab
      |aaabbababbbbababbbbbaaaababaaaaaababbaababaababbaababbbaaabbaaba
      |abbaabbbbabaabbbababbaab
      |baabbbbbbbaaabbbbababbbbbbaaaaaaabbbbbaaabaababa
      |aabbbbabbbbababbbabaaabbbabbbbbbbaaaabab
      |baabbbaabbabbbbababbbbaa
      |babbbbbbabbbbbbbaaabaabbbbbbbbbabbaaabaa
      |bbbabaaabbbbbaaaaaabbaab
      |abbbbbababaaabbaaababbbabbaaaaabaaaaabaabaaaaabb
      |bbabababaabbbaabaaabbbbabaaabbab
      |aaabbbbabbbbaaabbbbabbbbbbbbabbbaabababb
      |baaaabbaabbababbbabaaaaa
      |baababaabbaaaaabbababaabbbababbbababbbbabbabbaabbaaaabbabbaaabbbbbabbabbabbbabab
      |aaaaaaaaaabbbbababbbbabb
      |abbaaabbbaaaaaabbababaab
      |abaabbbbbaabbabaaaabababaabbababaaabbaab
      |aaaaabaabaababbaabbbbaba
      |bbbbbaaabbabababbaabbabaabaaaabaaabababb
      |aaaaabbaabbabaaaabbabbbabbbaaabbbaabaaaabbaaaaaa
      |aaabaaaaaaaaabbaaabaaabbbaabbbabaaaaabbbbabaaaaa
      |abaababbaababbbbaaabaaaabbbbbbbaabbaabaa
      |abaaabaabbbbbbabaabbbabbababaaba
      |bbabbaaabbaaaaababaaabbaaabbbabbbbbbabbaaabbabba
      |aaaaaaabbaababababaabbaababbaaaabaaabbab
      |bbbbbaaaaaababbbbaabbaab
      |abbaabbbbbaaaabbbbbbaaaabaabbabbaabbbaabbbaabbab
      |aaaabbbababaababbbbaababaababbaabbbabaababbbaabbbbaabbbaababbbaa
      |abbabbaaabbbaababbabaaab
      |aaaaaaababbaaabbbbaababb
      |baaabbaabaaabbaaabbbabba
      |babbbaabbbabbaabaabbaabaaabababa
      |aaaababbabababbbababaaabbbabaababbaababaaaabaabaabbabaaabbbaabaa
      |aaaaaaabaabbabaaabbabbaaaababbbbabbabaaabaaabaabaaaabbbaaabbbaaa
      |abbbbbbbbbbbabbabbbbaabb
      |baaabbbaabaababbaaabbaba
      |bbbbbbabbabbbabbaabaababbbbaabab
      |aaaaababbbbbabbbababbaabbbbbbabbaabbbbbbbbaaaaaa
      |ababaabbbababbbbbbbaaaabaabbbaaa
      |aaaababbbbbbabbabbaabbaa
      |aaabaababbbababbbabbabbb
      |bbababaaabaaabbaaabbaaaa
      |aaaaababbbaabbaaaaaaaabaabbbababbbbaabab
      |aabbaabbbabaabaabaaaaaaaabaababbbabbbbbaababbabbababaabaaabbbabbaabaabbaaabbbbba
      |bbbbaaaabbabababbbaaaaaa
      |abaaaaabaaaaaaabaabbaaabaaababaaaabaaaaa
      |babbbaabababaaabababbaab
      |baaaaaababababbbbaabababbbbbbaabbabaaaabaaaaaabaaababbaa
      |aabaaabbaababbabbabaabbbaababbabaabbbbaa
      |ababbbbbabbbaabababbaaba
      |bbababababbbaaaaabaabbabaaabbabb
      |bbaabaabaabbbaabbaaaaaabaabbabbbabbbbaab
      |baabaabbaababbbabbbbbaba
      |aaaaabababaaaaababababba
      |abababababbbabbbbababaaa
      |ababbbaabbabbbbabbbbbbaaabbbbabaabbabbba
      |babaaaaaaaabbaaabbbaabab
      |abaaababaaaabaaabaaaaaaabbabbbaaaababababaaaababaaaababbbbabaabbaabbbbba
      |aaababbbbaaaabbbaaaaaaabbbbabbbaaabbbbaa
      |babababaaaaababbabaabaab
      |bbaabbbababbabababaabaababbbbabaabbaaaaababbbbab
      |abababbbaabbbbababbbaaaaaaabaaab
      |aabaabaaaaaababaabbaaaabaabaaaaaaaababaa
      |aabbbaabbbbbabaababbabba
      |abbbaabaaaaaaaaababbbabbbbaaaababbbbbababbbaaaaa
      |aaabbbabbbbababbbbaabaabbaaaabab
      |aabbaababbabbbabbbbbbaba
      |abbaabbabbbbaaaaabaaaabbbaaababb
      |aaabababbbbbaaaabbbababb
      |abbbbbbbaaabaababbababababaabbbaabaabaaa
      |bbbabababaabbababaabbbbabaababaa
      |bbbabbabaaaabaaaaabbaaababababababababaaaaaabbbbaaaaabaabbaababb
      |aabaaabaaaaaaaababbabbbb
      |aabbabaabaabbabbaaabbbbaaabaabaa
      |bbaaaabbbbbabaaabababbbaaabaaababababaab
      |bbbbbbbaaabbbbbaaaaabbab
      |babaabbbbaaabaaabbbababbbbbaabaaabbbbaba
      |bbaaaabbbbbbaaaaabababaaaaabbaaabbbaabba
      |bbabbbababaabbbbaabaaabaaaabbaaa
      |baabaabbabaababbbbaabaabbaabbabbabbbbababbaaabbb
      |babbbabaabbbaababababbab
      |aabaaabbabaaaaabbbaaaabbaaabbaba
      |aaaaabaaabaabbbbaabbbbbb
      |aaaabbbabbababaabaabbbaabbbbbababababababbbaaabbaaabbabaaababbba
      |abaaaabbabbbbbabbbabbbababbaababbbaaabba
      |abababbbbaabbbbbbabbbbbbabbbbbabaaaababbababaabb
      |abaaabbabbababbbabaababbbaabbbbababbbaabbbaabbaa
      |babbbbbbbaabababbbababaaabbabbaabaabbabbaabbbaaa
      |aaababbababaaaabbbbaaaaa
      |babaaabbaabbbbbaaaaababa
      |aaabaaaaaabbaababaabbabababbaaaabbaabbaa
      |abaabbabbbaababaabbbaabababbabaabbaaaababbababaaabbbabbb
      |bbbbababbbaabaabbabaaabbaababbaaabaaabbb
      |baabbbabaaaaaaabbaabbaab
      |bbbaaabaaaababbbababaabbbbbbbaabbbbababb
      |babbaaaababbabbaabababbbababbbabaaababaa
      |aaabababaabbabaabbbabaaababbbbab
      |bbbbababbbabbbababbababbbaababaababaabba
      |bbbababababbbbbbabbbaaaaabaabbba
      |aabaaabbabbbbbaaabaababaabaaabab
      |bbabbabaabaaaaaababbbbaa
      |abaabaabaaaaabaabbbbbabababbabbbaabaabaa
      |bbabbbaababbbabbaabbaaabbabbabaabaaabaaababaabaabbbabbababbbbabbbabbabbabbaabbbb
      |aaaaabaababbababababbbbbaababbabbbabababbbbbabbb
      |abaaabbabbabbbaaaabababb
      |abbbbbababaaaaabbabaaaaa
      |bbbabbabaaabbaabbaaaabaababbaaab
      |bbbbbbbbabaabbaaaabbbaaa
      |baaabbbaaababbbbabbbabbbabaabaabaababaab
      |aaaababbbbaabbbaaaaababbbabbabba
      |bbabbbaaababaaabbbbbbbbabbaabaababbabbba
      |aaabababbabbbabaaabbabaaaaabbaba
      |aabaabbbbaabbbabaaabbbbabbbbabbb
      |bbbbbbabababaaababaabbbbaabbaaaabababbbb
      |abaaaabbaaabaabbbbaaababaaabbaabaabbbbbb
      |baabbababbbaaabaababbbba
      |baaabbbabababbbaabbaabab
      |babababaaaabaaaabbbbabbb
      |babbababaaabbbbabbbaabaabaabbbbbaaabbbab
      |baabbaabbababbababbaaaaa
      |bbababbbaaaaaaaabaabbbbaaaaabaaa
      |abbbaabbbbbbbbaabbabbbabbbbbabaaababaaab
      |bbaabbbababaabbbbabaaaba
      |baaababababaabbbbbabbaba
      |abababbbaababbbbabbabaabbaabbbaaabbbaabaabbbbaaa
      |bbaaaaaaaaabababbaabababaababbbbbbbbabaaaaabbabbbbbbbbaabaaaaabbbababbaaaaababaa
      |baaaaabaaaaaaaaaabbabbba
      |baabbbbaabaabbabbaaabbbb
      |abbabbabaabbabaaaabbabab
      |abaabbbbbaaaaababbabaaba
      |bbbbbaaabbaabbbaabaaabbaabaaaaabbaabababaabbabbbbabbaabb
      |babbbabaabaaabbaabbbbbaa
      |bbabbaabbabbabaabbaaaabaaaabbaab
      |aaaababaaabaaaaabbabbabaababaabaaabbabbb
      |abbaaabbbbababaabaaaabaa
      |baabbbbabaabaabbbaabbaab
      |baababbabbbabaabaaabaabbbaaabaaabbbbbaba
      |bbbababbaabbbabbbbbbbbaa
      |bbbbbbabbbabbabbabbaabbbaaabaaab
      |bbbababbababaaababaaabbabaababbb
      |baabaabbaabbabaabbbaaaab
      |bbaaaababbabababaaabaaaaaaabaabaabbbbaba
      |bbabbaaabbbbbaaabbbaabab
      |ababaaababbabababbaababb
      |aabaaaababbabbabbabbbbaa
      |aaaabaaabbaabbabababaaaaabbbabbb
      |abbbabaabbabbbaaaabaaababbabbabbbbaabaababaaabbb
      |bbaaaabbaabbaaabaaaaaaabaaabbbbaaabbaabaaababaaabbaaabaa
      |babbbabbaaabababbbabaaab
      |abbabbabbaabbababbabbbbaaaaabbbabbababba
      |abbaababbbaabbababbbabbaaabbbbbaaaabababaabababbbbaabbbaabbabaabbbbbabbaabaaaaab
      |babbbabaababbbabbbbabbba
      |baaabaabaaaaaaabbabaabba
      |baabaabbbaabaaabbbabbaaabaaaaaabbbbbaabb
      |bbababbbabaaaabbbababaaa
      |baababbaaaabbbbaabbababbaabbaabaabaabbbabbabaaab
      |bbbbbbaaabbbababbaababbbbaaabbaabbaababb
      |bbabbaaabaabbbaababbaabb
      |ababbbababbbbbbbababbabb
      |babababababaaaaaaaaababababbaaabbabaabab
      |abaaabaaabbaabbbbabbaaba
      |aaaababbbabbbbbbbbbaabba
      |babbbabaaababaaabaaababb
      |ababbbabbbabbabbbbababaaaaabbbaa
      |aaaaabbbaabbaababbbbbbbbbaaaabbbaaaaabaabbbbaabaaababbaabababbabbabbabbb
      |abbabaabbbabbbaaaaabaaaabbbbbaaa
      |bbbbabbaaabaabbbaabababb
      |bbbabbaabbbaabaaabaaabaabaaaaaabaabbbbaa
      |bbbababbbabaaaabaaabbaba
      |abbababbabaababbbaabbabaaaabaabbaaaabaaabbabbbbbababababbbbaabba
      |aaabaaaaaababbbaabbaabab
      |abaabbaabaaabababaaabbbababbbbab
      |aaaaabbbbbababbbbabaaaaa
      |aabaaabaaababaaaabaabaaa
      |aabaabababbbaaaabaabbaaa
      |babaabaababaabbbbbababbbaaababbaabbabaabbabbbbabbaabaaba
      |aabaaabaaaabaaaabbaabbbb
      |bbbbaabaabaaabaaaababbbabaabbaab
      |bbabababbbabbbbaabbbbaab
      |baabababbaabbbbbbaaabbbabbbabbbbbabaabbababbaaabaabababa
      |abbbabbbabbabbabbaaaabaabbbbbaaaabaabababaababaaaaabababaababbba
      |baabbabbbaabbbbaabaabbabaababbaa
      |abaababbaaaaabbbaabbbaabaaababbbbbaaababaaaaababbabbaabb
      |bbbabbbbababababaaabaaabbaabbabaabbabbbbaababaaaabbbabbbbbbabbbbaaaaababbaaaaaaa
      |aababbabaabababaaababbabbaaaaaaabbbbbbaabbaaabbbbbbaabbb
      |ababbbabaaabaababbbaaabb
      |abbbbbbaabbbbbbbbbababba
      |bbabbbbabbabbbaaabbabaababbbaaaaaabbaaaa
      |bbaabaababbbbbbbabaabaab
      |aaaaabbabbbbabbabaabbbbbbabbabaaababbabbaabbaaaa
      |abbaabbabbbbbbbbabababba
      |abababbbabbababbabbabbaabbbbaabb
      |baaabaaaaaaaabbabaaababb
      |abaabbbbbabaaaabbaaaabaa
      |bbaabaaabbaabaaaabbbbbaa
      |baabaaabbbbbbbbabbababaaabbaaaab
      |bbaaabababbaaabbaabaaabbaabaabbabaaaabaa
      |aababbbbbbabbabbbbabbbbaaabababb
      |abbbabaabbaaaaabaaabbaaa
      |abbbaaaabaaabaaaaaaaaaaabbaabbab
      |aaabaaaabbbbbbbaabbababbbabbbabaabbabbaabbbbaaaabbbaabbb
      |aabbaaabbbabbabbbbbaabaaaaaaaabaabbbaabb
      |babbbbbbaabbabaabaaaabab
      |aaaaabbbaaaaabbaaababbbaabababab
      |baabaabbbbbbbbbabbaabaaababaabab
      |aababbbbaaaabbaababaaaaabaabbbabbbababaaaabaaabbaaaabaabababbbabbabaabbabaabaaaa
      |aababaaaabbbababbbaabaabbbbababbbbaaabababababba
      |bbbbbbabbabbababaaaaabaabaabbbaabbaaabababbaaaab
      |abbaabbbaabbbbabbbabbaba
      |bbaababaabaabbbbbabababb
      |aabaabbbbbbbbbbababbababbbbbababbabbaaab
      |bbabbaabaabbaabababaaaabbabbaaaaabaaabab
      |babbaaaabbabbabbabbaabab
      |bbbababbabbababbbabbaabb
      |aaababbabaaaaaababbbabba
      |babaabaabaaababaaabaaabaaaabbabb
      |""".stripMargin
