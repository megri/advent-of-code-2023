import java.util.Comparator
def input = os.read(os.pwd / "inputs" / "day07.txt").linesIterator


def testInput =
    """|32T3K 765
       |T55J5 684
       |KK677 28
       |KTJJT 220
       |QQQJA 483""".stripMargin.linesIterator


def naturalOrder(card: Char) = card match
    case 'A'   => 14
    case 'K'   => 13
    case 'Q'   => 12
    case 'J'   => 11
    case 'T'   => 10
    case other => other.asDigit


def jacksAreWorth1(card: Char) = card match
    case 'A'   => 14
    case 'K'   => 13
    case 'Q'   => 12
    case 'J'   => 1
    case 'T'   => 10
    case other => other.asDigit


case class Hand(cards: String, bid: Int, labelGroupsTransformer: Map[Char, Int] => Map[Char, Int]):
    val labelGroups = labelGroupsTransformer(cards.groupMapReduce(identity)(_ => 1)(_ + _))

    def strength: Int =
        (labelGroups.values.toList.filterNot(_ == 1).sorted: @unchecked) match
            case List(5)    => 6
            case List(4)    => 5
            case List(2, 3) => 4
            case List(3)    => 3
            case List(2, 2) => 2
            case List(2)    => 1
            case List()     => 0


def orderByHighestValue(h1: String, h2: String, cardValueF: Char => Int) =
    h1.iterator
        .zip(h2.iterator)
        .map((c1, c2) => (cardValueF(c1), cardValueF(c2)))
        .dropWhile(_ - _ == 0)
        .map(_ < _)
        .nextOption()
        .getOrElse(false)


def parse(input: Iterator[String], labelGroupsTransformer: Map[Char, Int] => Map[Char, Int]) =
    input.map: line =>
        val Array(cards, bid) = line.split(' '): @unchecked
        Hand(cards, bid.toInt, labelGroupsTransformer)


def rank(input: Iterator[String], labelGroupsTransformer: Map[Char, Int] => Map[Char, Int], cardValueF: Char => Int) =
    parse(input, labelGroupsTransformer).toList
        .sortWith: (h1, h2) =>
            if h1.strength == h2.strength then orderByHighestValue(h1.cards, h2.cards, cardValueF)
            else h1.strength < h2.strength
        .zipWithIndex
        .map((hand, rank) => hand.bid * (rank + 1))
        .sum


val s1 = rank(input, identity, naturalOrder)

println(s"Solution 1: $s1.")


def jacksToBest(labels: Map[Char, Int]): Map[Char, Int] =
    val jackCount = labels.getOrElse('J', 0)
    val withoutJacks = labels.filterNot((k, _) => k == 'J')
    if withoutJacks.isEmpty then labels
    else
        val largestGroup = withoutJacks.maxBy((k, v) => v)
        withoutJacks.updated(largestGroup._1, largestGroup._2 + jackCount)


val s2 = rank(input, jacksToBest, jacksAreWorth1)

println(s"Solution 2: $s2.")
