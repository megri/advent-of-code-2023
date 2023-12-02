def input = os.read(os.pwd / "inputs" / "day02.txt").linesIterator

type RGBSet = (Int, Int, Int)


case class Game(id: Int, sets: List[RGBSet]):
    def power: Int =
        val (r, g, b) = sets.foldLeft((0, 0, 0)):
            case ((maxR, maxG, maxB), (r, g, b)) =>
                (maxR max r, maxG max g, maxB max b)

        r * g * b


def parse(row: String): Game =
    // pattern: Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    val Array(game, gameData) = row.split(':')
    val rgbSets =
        for sets <- gameData.split(';')
        yield
            val setParts = sets.split(',').map(_.trim)
            setParts.foldLeft((0, 0, 0)):
                case ((r, g, b), s"$n red")   => (n.toInt, g, b)
                case ((r, g, b), s"$n green") => (r, n.toInt, b)
                case ((r, g, b), s"$n blue")  => (r, g, n.toInt)

    game match
        case s"Game $id" => Game(id.toInt, rgbSets.toList)


def parsedInput = input.map(parse)


val s1 = parsedInput
    .filter: game =>
        game.sets.forall: (r, g, b) =>
            r <= 12 && g <= 13 && b <= 14
    .map(_.id)
    .sum


println(s"Solution 1: $s1")

val s2 = parsedInput.map(_.power).sum

println(s"Solution 2: $s2")
