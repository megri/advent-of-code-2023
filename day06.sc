def testInput =
    """|Time:      7  15   30
       |Distance:  9  40  200""".stripMargin.linesIterator


def input = os.read(os.pwd / "inputs" / "day06.txt").linesIterator


case class RaceRecord(time: Long, distance: Long):
    def winningRaces: Iterator[Long] =
        Iterator
            .iterate(0L)(_ + 1)
            .takeWhile(_ < time)
            .filter(candidate => candidate * (time - candidate) > distance)


def parse[A](input: Iterator[String]): (Iterator[String], Iterator[String]) =
    def nums(s: String) = s.split(raw"\s+").iterator.drop(1)
    (nums(input.next()), nums(input.next()))


val s1 =
    val (ts, ds) = parse(input)
    val races = ts.zip(ds).map((t, d) => RaceRecord(t.toLong, d.toLong))
    races.map(_.winningRaces.size).product


println(s"Solution 1: $s1.")


val s2 =
    val (ts, ds) = parse(input)
    RaceRecord(ts.mkString.toLong, ds.mkString.toLong).winningRaces.size


println(s"Solution 2: $s2.")
