//> using scala 3.3.1
//> using toolkit latest

def input = os.read(os.pwd / "inputs" / "day01.txt").linesIterator


def firstAndLastDigitNumber(row: String): Int =
    val opt = for
        firstDigitChar <- row.find(_.isDigit)
        lastDigitChar <- row.findLast(_.isDigit)
    yield
        val firstDigit = firstDigitChar.asDigit
        val lastDigit = lastDigitChar.asDigit
        firstDigit * 10 + lastDigit
    opt.get


val s1 = input.map(firstAndLastDigitNumber).sum

println(s"Solution 1: $s1.")


val digitWords = Map(
    "one" -> '1',
    "two" -> '2',
    "three" -> '3',
    "four" -> '4',
    "five" -> '5',
    "six" -> '6',
    "seven" -> '7',
    "eight" -> '8',
    "nine" -> '9'
)


def transformDigitWords(row: String): String =
    val res = row.indices.flatMap: i =>
        if row(i).isDigit then Some(row(i))
        else
            digitWords.keys.collectFirst:
                case key if row.substring(i).startsWith(key) => digitWords(key)

    res.mkString


val s2 = input.map(transformDigitWords).map(firstAndLastDigitNumber).sum

println(s"Solution 2: $s2.")
