def input = os.read(os.pwd / "inputs" / "day09.txt").linesIterator


def testInput =
    """|0 3 6 9 12 15
       |1 3 6 10 15 21
       |10 13 16 21 30 45""".stripMargin.linesIterator


def parse(line: String): List[Int] =
    line.split(' ').iterator.map(_.toInt).toList


def changes(input: List[Int]): List[Int] =
    input.tail.zip(input).map(_ - _).toList


def extrapolate(transform: (List[Int], List[Int]) => List[Int])(input: List[Int]): List[Int] =
    if input.forall(_ == 0) then input
    else transform(input, extrapolate(transform)(changes(input)))


def solve(input: Iterator[String])(transform: (List[Int], List[Int]) => List[Int]) =
    input.map(parse andThen extrapolate(transform))


val s1 =
    val c = solve(input): (orig, d) =>
        orig.appended(orig.last + d.last)

    c.map(_.last).sum


println(s"Solution 1: $s1.")


val s2 =
    val c = solve(input): (orig, d) =>
        (orig.head - d.head) :: orig

    c.map(_.head).sum


println(s"Solution 2: $s2.")
