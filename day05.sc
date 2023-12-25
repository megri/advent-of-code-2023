case class Range private (start: Long, steps: Long):
    def end = start + steps

    def nonEmpty = steps > 0

    def isEmpty = !nonEmpty

    def splitAt(pos: Long): (Range, Range) =
        val cutoffPoint = pos.min(end).max(start)
        val r1 = Range(start, cutoffPoint - start)
        val r2 = Range(cutoffPoint, end - cutoffPoint)
        (r1, r2)

    def translate(shift: Long): Range = copy(start = start + shift)

    override def toString: String =
        if nonEmpty then s"[$start, $end)"
        else "[empty)"


object Range:
    val empty = new Range(0, 0)

    def apply(start: Long, steps: Long): Range =
        if steps > 0 then new Range(start, steps) else empty


case class RangeMapping(dst: Long, src: Long, steps: Long):
    def translate(range: Range): (Range, Range, Range) =
        val (below, rest) = range.splitAt(src)
        val (overlap, above) = rest.splitAt(src + steps)
        (below, overlap.translate(dst - src), above)


def input = os.read(os.pwd / "inputs" / "day05.txt").linesIterator


def testInput =
    """|seeds: 79 14 55 13
       |
       |seed-to-soil map:
       |50 98 2
       |52 50 48
       |
       |soil-to-fertilizer map:
       |0 15 37
       |37 52 2
       |39 0 15
       |
       |fertilizer-to-water map:
       |49 53 8
       |0 11 42
       |42 0 7
       |57 7 4
       |
       |water-to-light map:
       |88 18 7
       |18 25 70
       |
       |light-to-temperature map:
       |45 77 23
       |81 45 19
       |68 64 13
       |
       |temperature-to-humidity map:
       |0 69 1
       |1 0 69
       |
       |humidity-to-location map:
       |60 56 37
       |56 93 4""".stripMargin.linesIterator


def parse(input: Iterator[String], seedsAsIntervals: Boolean = false): (List[Range], List[List[RangeMapping]]) =
    val seedsParts = input.next().drop("seeds: ".size).split(' ').iterator

    val seeds =
        if seedsAsIntervals then
            seedsParts
                .grouped(2)
                .foldLeft(List.empty[Range]): (acc, group) =>
                    val start = group(0).toLong
                    val steps = group(1).toLong
                    Range(start, steps) :: acc
        else
            seedsParts
                .foldLeft(List.empty[Range]):
                    case (acc, n) =>
                        val start = n.toLong
                        Range(start, 1) :: acc

    val rangeMappings = input.foldLeft(List.empty[List[RangeMapping]]):
        case (mappings, s"$source-to-$target map:") =>
            Nil :: mappings

        case (segment :: mappings, s"$dstPart $srcPart $stepsPart") =>
            (RangeMapping(dstPart.toLong, srcPart.toLong, stepsPart.toLong) :: segment) :: mappings

        case (mappings, "") => mappings

        case (_, unexpected) => sys.error(s"parse error: did not expect line '$unexpected'.")

    (seeds.reverse, rangeMappings.map(_.reverse).reverse)


def processSection(r: Range, section: List[RangeMapping]): List[Range] =
    if r.isEmpty then List(Range.empty)
    else
        section match
            case rm :: rest =>
                val (below, overlap, above) = rm.translate(r)
                val recursed = processSection(below, rest) ::: processSection(above, rest)
                (overlap :: recursed).filter(_.nonEmpty)
            case Nil => List(r)


def solve(seeds: List[Range], rangeMappingSections: List[List[RangeMapping]]) =
    seeds
        .flatMap: seed =>
            rangeMappingSections.foldLeft(List(seed)):
                case (prev, section) =>
                    prev.flatMap(processSection(_, section))
        .map(_.start)
        .min


val s1 =
    val (seeds, rms) = parse(input, seedsAsIntervals = false)
    solve(seeds, rms)


println(s"Solution 1: $s1.")


val s2 =
    val (seeds, rms) = parse(input, seedsAsIntervals = true)
    solve(seeds, rms)


println(s"Solution 2: $s2.")
