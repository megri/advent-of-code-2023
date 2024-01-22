def input = os.read(os.pwd / "inputs" / "day08.txt").linesIterator


def testInput =
    """|LLR
       |
       |AAA = (BBB, BBB)
       |BBB = (AAA, ZZZ)
       |ZZZ = (ZZZ, ZZZ)""".stripMargin.linesIterator


def parse(lines: Iterator[String]) =
    val turns = lines.next()
    lines.next()
    val map = lines
        .map:
            case s"$from = ($left, $right)" =>
                (from, (left, right))
            case other => sys.error(s"Did not expect '$other'")
        .toMap

    turns -> map


val s1 =
    val (turns, map) = parse(input)
    val repeatedTurns = Iterator.continually(turns).flatten
    val stepper = repeatedTurns.scanLeft("AAA"):
        case (curr, turn) => if turn == 'L' then map(curr)._1 else map(curr)._2

    stepper.zipWithIndex.dropWhile(_._1 != "ZZZ").next()._2


println(s"Solution 1: $s1.")


def testInput2 =
    """|LR
       |
       |11A = (11B, XXX)
       |11B = (XXX, 11Z)
       |11Z = (11B, XXX)
       |22A = (22B, XXX)
       |22B = (22C, 22C)
       |22C = (22Z, 22Z)
       |22Z = (22B, 22B)
       |XXX = (XXX, XXX)""".stripMargin.linesIterator


def lcm(a: Long, b: Long): Long =
    def gcd(a: Long, b: Long): Long =
        if b == 0 then a
        else gcd(b, a % b)
    (a * b) / gcd(a, b)


val s2 =
    val (turns, map) = parse(input)
    val starts = map.filterKeys(_.endsWith("A")).keys.toList
    def repeatedTurns = Iterator.continually(turns).flatten

    starts
        .map: start =>
            val found = repeatedTurns
                .scanLeft(start):
                    case (curr, turn) => if turn == 'L' then map(curr)._1 else map(curr)._2
                .zipWithIndex
                .dropWhile(!_._1.endsWith("Z"))
                .next()

            found._2.toLong
        .reduce(lcm)


println(s"Solution 2: $s2.")
