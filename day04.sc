import scala.collection.mutable


val testInput =
    """|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
       |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
       |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
       |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
       |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
       |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11""".stripMargin.linesIterator.toArray


def input = os.read(os.pwd / "inputs" / "day04.txt").linesIterator


case class Card(id: Int, winning: List[Int], nums: List[Int]):
    def matching = nums.filter(winning.contains).size

    def score: Int =
        if matching == 0 then 0 else math.pow(2, matching - 1).toInt

    override def toString: String = s"Card $id: $score"


def parse(line: String): Card =
    def parseCard(c: String): Int =
        c match
            case s"Card $n" => n.trim.toInt

    def parseNumbers(ns: String): List[Int] =
        ns.split(raw"\s+").toList.filter(_.nonEmpty).map(_.toInt)

    val Array(cardPart, numbersPart) = line.split(':')
    val Array(winning, mine) = numbersPart.split('|').map(parseNumbers)

    Card(parseCard(cardPart), winning, mine)


val s1 = input.map(parse(_).score).sum

println(s"Solution 1: $s1")


val s2 =
    val cards = input.map(parse).toList
    val cardCounts = mutable.Map.from(cards.map(card => card.id -> 1))

    for card <- cards do
        val bonusCardIds = card.id + 1 to card.id + card.matching
        bonusCardIds.foreach: id =>
            cardCounts(id) += cardCounts(card.id)

    cardCounts.values.sum


println(s"Solution 2: $s2")
