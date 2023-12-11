def input = os.read(os.pwd / "inputs" / "day03.txt").linesIterator

val grid = input.toArray

val height = grid.size
val width = grid(0).size


case class Pos(row: Int, col: Int):
    def neighbours(that: Pos): Boolean =
        (this.row - that.row).abs < 2 &&
            (this.col - that.col).abs < 2


def isSymbol(pos: Pos): Boolean =
    val ch = grid(pos.row)(pos.col)
    !ch.isDigit && ch != '.'


def isGear(pos: Pos): Boolean =
    grid(pos.row)(pos.col) == '*'


def allPositions =
    for
        row <- Iterator.from(0).take(height)
        col <- Iterator.from(0).take(width)
    yield Pos(row, col)


val numbersWithPositions =
    grid.indices
        .flatMap: row =>
            val digits = grid(row).split("[^0-9]")
            val cols = digits.iterator.scanLeft(0): (c, ns) =>
                c + ns.size + 1
            digits.iterator
                .zip(cols)
                .collect:
                    case (ns, c) if ns.nonEmpty =>
                        val coords = (c to c + ns.size - 1).map(Pos(row, _)).toSet
                        (ns.toInt, coords)
        .toList


def numbersNeighbouringPosition(posFilter: Pos => Boolean): Iterator[List[Int]] =
    allPositions
        .filter(posFilter)
        .map: pos =>
            numbersWithPositions.collect:
                case (n, nps) if nps.exists(numberPos => numberPos.neighbours(pos)) => n


val s1 = numbersNeighbouringPosition(isSymbol).flatten.sum

println(s"Solution 1: $s1")


val s2 = numbersNeighbouringPosition(isGear)
    .flatMap:
        case List(a, b) => List(a * b)
        case _          => Nil
    .sum

println(s"Solution 2: $s2")
