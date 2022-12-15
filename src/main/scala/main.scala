import scala.io.Source
import java.util.PriorityQueue
import java.util.ArrayDeque
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util._

trait Puzzle {
  def getDataFilePath: String =
    s"./data/${getClass.getSimpleName.reverse.dropWhile(!_.isDigit).reverse}"

  def tests: Seq[(String, Any)] = Nil

  def main(args: Array[String]): Unit = {
    tests.zipWithIndex.foreach { case ((input, expected), idx) =>
      val result = solve(
        input.split("\n").iterator
      )
      val str = if (result == expected) { s"Success! $expected" }
      else { s"FAIL!! expected $expected, but calculated $result" }

      println(s"test $idx: $str")
    }

    val inputFile = args.headOption.getOrElse {
      getDataFilePath
    }
    val input = Source.fromFile(inputFile)
    println(solve(input.getLines))
  }

  def solve(lines: Iterator[String]): Any
}

object day1a extends Puzzle {
  override def tests = Seq(
    (
      """1000
       |2000
       |3000
       |
       |4000
       |
       |5000
       |6000
       |
       |7000
       |8000
       |9000
       |
       |10000""".stripMargin,
      24000
    )
  )
  def solve(input: Iterator[String]): Int = {
    var max = 0
    var current = 0
    input
      .foreach { line =>
        line match {
          case "" =>
            if (max < current) {
              max = current
            }
            current = 0
          case x =>
            current += x.toInt
        }
      }
    if (max < current) {
      max = current
    }

    max
  }
}

object day1b extends Puzzle {
  override def tests = Seq(
    (
      """1000
       |2000
       |3000
       |
       |4000
       |
       |5000
       |6000
       |
       |7000
       |8000
       |9000
       |
       |10000""".stripMargin,
      45000
    )
  )

  def solve(input: Iterator[String]): Int = {
    var current = 0
    val queue = new PriorityQueue[Int]
    input
      .foreach { line =>
        line match {
          case "" =>
            queue.add(current)
            if (queue.size > 3) queue.poll()
            current = 0
          case x =>
            current += x.toInt
        }
      }
    queue.add(current)
    if (queue.size > 3) queue.poll()

    queue.iterator().asScala.sum
  }
}

object day2a extends Puzzle {
  def solve(input: Iterator[String]): Int = {
    var score = 0
    input
      .foreach { line =>
        line.split(" ") match {
          case Array(opponent, me) =>
            score += (me match {
              case "X" => 1
              case "Y" => 2
              case "Z" => 3
              case _   => 0
            })
            score += ((me, opponent) match {
              case ("X", "A") => 3
              case ("X", "B") => 0
              case ("X", "C") => 6
              case ("Y", "A") => 6
              case ("Y", "B") => 3
              case ("Y", "C") => 0
              case ("Z", "A") => 0
              case ("Z", "B") => 6
              case ("Z", "C") => 3
              case _          => 0
            })
        }
      }

    score
  }
}

object day2b extends Puzzle {
  def solve(input: Iterator[String]): Int = {
    var score = 0
    input
      .foreach { line =>
        line.split(" ") match {
          case Array(opponent, me) =>
            score += ((me, opponent) match {
              case ("X", "A") => 3
              case ("X", "B") => 1
              case ("X", "C") => 2
              case ("Y", "A") => 1 + 3
              case ("Y", "B") => 2 + 3
              case ("Y", "C") => 3 + 3
              case ("Z", "A") => 2 + 6
              case ("Z", "B") => 3 + 6
              case ("Z", "C") => 1 + 6
              case _          => 0
            })
        }
      }

    score
  }
}

object day3a extends Puzzle {
  def solve(input: Iterator[String]): Int = {
    input.map { string =>
      val len = string.length / 2
      val left = string.take(len).toSet
      val right = string.drop(len).toSet
      val single = left.intersect(right).toIterator.next()
      if (single.toInt > 'a')
        (single.toInt - 'a'.toInt) + 1
      else (single.toInt - 'A'.toInt + 27)
    }.sum
  }
}

object day3b extends Puzzle {
  def solve(input: Iterator[String]): Int = {
    input
      .grouped(3)
      .map(_.map(_.toSet).reduce(_ intersect _).head)
      .map { single =>
        if (single.toInt > 'a'.toInt)
          single.toInt - 'a'.toInt + 1
        else
          single.toInt - 'A'.toInt + 27
      }
      .sum
  }
}

object day4a extends Puzzle {
  def getRange(str: String): (Int, Int) = {
    str.split('-') match {
      case Array(start, end) => (start.toInt, end.toInt)
      case _                 => ???
    }
  }

  override def tests = Seq(
    (
      """2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8""",
      2
    )
  )
  def solve(input: Iterator[String]): Int = {
    input
      .map(_.split(','))
      .map { case Array(left, right) =>
        val (l1, l2) = getRange(left)
        val (r1, r2) = getRange(right)
        if (l1 >= r1 && l2 <= r2) {
          1
        } else if (r1 >= l1 && r2 <= l2) {
          1
        } else {
          0
        }
      }
      .sum
  }
}

object day4b extends Puzzle {
  import day4a.getRange

  override def tests = Seq(
    (
      """2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8""",
      4
    )
  )
  def solve(input: Iterator[String]): Int = {
    input
      .map(_.split(','))
      .map { case Array(left, right) =>
        val (l1, l2) = getRange(left)
        val (r1, r2) = getRange(right)
        if (l1 > r2 || r1 > l2) { 0 }
        else { 1 }
      }
      .sum
  }
}

object day5a extends Puzzle {
  override def tests = Seq(
    (
      """    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2""",
      "CMZ"
    )
  )

  val MOVE_REGEX = "move (\\d+) from (\\d) to (\\d)".r

  def solve(input: Iterator[String]): String = {
    val stacks = Array.fill(10)(new ArrayDeque[Char])
    input.takeWhile(_.nonEmpty).foreach { line =>
      line.zipWithIndex.foreach { (c, i) =>
        if (c.isLetter) {
          val stackIdx = (i - 1) / 4
          stacks(stackIdx).addFirst(c)
        }
      }
    }
    def printStacks = println(
      stacks.map { s => s.toArray.mkString }.mkString("\n")
    )

    input
      .foreach { case MOVE_REGEX(num, from, to) =>
        val n = num.toInt
        val f = stacks(from.toInt - 1)
        val t = stacks(to.toInt - 1)

        (0 until n).foreach { _ => t.addLast(f.removeLast()) }
      }
    stacks.flatMap(s => if (s.isEmpty) None else Some(s.getLast)).mkString("")
  }
}

object day5b extends Puzzle {
  override def tests = Seq(
    (
      """    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2""",
      "MCD"
    )
  )

  val MOVE_REGEX = "move (\\d+) from (\\d) to (\\d)".r

  def solve(input: Iterator[String]): String = {
    val stacks = Array.fill(10)(new ArrayDeque[Char])
    input.takeWhile(_.nonEmpty).foreach { line =>
      line.zipWithIndex.foreach { (c, i) =>
        if (c.isLetter) {
          val stackIdx = (i - 1) / 4
          stacks(stackIdx).addFirst(c)
        }
      }
    }
    def printStacks = println(
      stacks.map { s => s.toArray.mkString }.mkString("\n")
    )

    input
      .foreach { case MOVE_REGEX(num, from, to) =>
        val n = num.toInt
        val f = stacks(from.toInt - 1)
        val t = stacks(to.toInt - 1)
        val tmp = new ArrayDeque[Char]

        (0 until n).foreach { _ => tmp.addLast(f.removeLast()) }
        (0 until n).foreach { _ => t.addLast(tmp.removeLast()) }
      }
    stacks.flatMap(s => if (s.isEmpty) None else Some(s.getLast)).mkString("")
  }
}

object day6a extends Puzzle {
  override def tests = Seq(
    (
      """bvwbjplbgvbhsrlpgdmjqwftvncz""",
      5
    )
  )
  def solve(input: Iterator[String]): Int = {
    val current = new java.util.ArrayDeque[Char]
    input.next.zipWithIndex.foreach { case (char, i) =>
      current.addLast(char)
      if (current.size > 4) {
        current.removeFirst()
      }
      if (current.iterator.asScala.toSet.size == 4) {
        return i + 1
      }
    }
    0
  }
}

object day6b extends Puzzle {
  override def tests = Seq(
    (
      """bvwbjplbgvbhsrlpgdmjqwftvncz""",
      23
    )
  )
  def solve(input: Iterator[String]): Int = {
    val current = new java.util.ArrayDeque[Char]
    input.next.zipWithIndex.foreach { case (char, i) =>
      current.addLast(char)
      if (current.size > 14) {
        current.removeFirst()
      }
      if (current.iterator.asScala.toSet.size == 14) {
        return i + 1
      }
    }
    0
  }
}

object day7a extends Puzzle {
  override def tests = Seq(
    (
      """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k""",
      95437
    )
  )

  val CD = "\\$ cd (.*)".r
  val LS = "$ ls"
  val DIR = "dir (.*)".r
  val FILE = "(\\d+) (.*)".r

  def solve(input: Iterator[String]): Int = {
    var currentDir = "/"
    def allParents = {
      Iterator
        .iterate(currentDir) { d =>
          d.take(d.lastIndexOf('/'))
        }
        .takeWhile(_.nonEmpty)
    }
    val dirSizes = mutable.Map[String, Int]("/" -> 0)

    input.foreach {
      case CD(newDir) =>
        if (newDir == "/") {
          currentDir = "/"
        } else if (newDir == "..") {
          currentDir = currentDir.take(currentDir.lastIndexOf('/'))
        } else {
          currentDir += "/" + newDir
        }
      case LS =>
      case FILE(size, name) =>
        allParents.foreach { dir =>
          dirSizes(dir) = dirSizes(dir) + size.toInt
        }
      case DIR(name) =>
        dirSizes += (currentDir + "/" + name) -> 0
    }

    dirSizes.map(_._2).filter(_ <= 100000).sum
  }
}

object day7b extends Puzzle {
  override def tests = Seq(
    (
      """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k""",
      24933642
    )
  )

  val CD = "\\$ cd (.*)".r
  val LS = "$ ls"
  val DIR = "dir (.*)".r
  val FILE = "(\\d+) (.*)".r

  def solve(input: Iterator[String]): Int = {
    var currentDir = "/"
    def allParents = {
      Iterator
        .iterate(currentDir) { d =>
          d.take(d.lastIndexOf('/'))
        }
        .takeWhile(_.nonEmpty)
    }
    val dirSizes = mutable.Map[String, Int]("/" -> 0)
    var totalSize = 0

    input.foreach {
      case CD(newDir) =>
        if (newDir == "/") {
          currentDir = "/"
        } else if (newDir == "..") {
          currentDir = currentDir.take(currentDir.lastIndexOf('/'))
        } else {
          currentDir += "/" + newDir
        }
      case LS =>
      case FILE(sizeStr, name) =>
        val size = sizeStr.toInt
        totalSize += size
        allParents.foreach { dir =>
          dirSizes(dir) = dirSizes(dir) + size.toInt
        }
      case DIR(name) =>
        dirSizes += (currentDir + "/" + name) -> 0
    }

    val spaceLeft = 70000000 - totalSize
    val numToDelete = 30000000 - spaceLeft
    dirSizes.toSeq.map(_._2).sorted.iterator.filter(_ >= numToDelete).min
  }
}

object day8a extends Puzzle {
  override val tests = Seq(
    (
      """30373
25512
65332
33549
35390""",
      21
    )
  )
  def solve(input: Iterator[String]): Int = {
    val grid = input.map(_.map(_.toInt).toArray).toArray
    val width = grid.head.length
    val height = grid.length

    val fromBottom = Array.fill(height)(Array.fill(width)(false))
    val fromTop = Array.fill(height)(Array.fill(width)(false))
    val fromLeft = Array.fill(height)(Array.fill(width)(false))
    val fromRight = Array.fill(height)(Array.fill(width)(false))

    (0 until width).foreach { i =>
      var max = -1
      (0 until height).foreach { j =>
        val cur = grid(j)(i)
        if (cur > max) {
          fromTop(j)(i) = true
          max = cur
        }
      }
    }
    (0 until width).foreach { i =>
      var max = -1
      (0 until height).foreach { negJ =>
        val j = height - 1 - negJ
        val cur = grid(j)(i)
        if (cur > max) {
          fromBottom(j)(i) = true
          max = cur
        }
      }
    }

    (0 until height).foreach { j =>
      var max = -1
      (0 until width).foreach { negI =>
        val i = width - 1 - negI
        val cur = grid(j)(i)
        if (cur > max) {
          fromLeft(j)(i) = true
          max = cur
        }
      }
    }
    (0 until height).foreach { j =>
      var max = -1
      (0 until width).foreach { i =>
        val cur = grid(j)(i)
        if (cur > max) {
          fromRight(j)(i) = true
          max = cur
        }
      }
    }

    var visibleTrees = 0
    for {
      i <- 0 until width
      j <- 0 until height
    } {
      if (
        fromBottom(i)(j) || fromTop(i)(j) || fromLeft(i)(j) || fromRight(i)(j)
      ) {
        visibleTrees += 1
      }
    }

    visibleTrees
  }
}

object day8b extends Puzzle {
  override val tests = Seq(
    (
      """30373
25512
65332
33549
35390""",
      8
    )
  )

  def solve(lines: Iterator[String]): Int = {
    val grid = lines.map(_.map(_.toInt - '0'.toInt).toArray).toArray
    val width = grid.head.length
    val height = grid.length
    var max = 0
    for {
      i <- 0 until width
      j <- 0 until height
    } {
      val cur = grid(j)(i)
      val up = Iterator
        .iterate(j - 1)(_ - 1)
        .takeWhile(_ >= 0)
        .find(j2 => j2 == 0 || grid(j2)(i) >= cur)
        .map(j - _)
        .getOrElse(0)
      val down = Iterator
        .iterate(j + 1)(_ + 1)
        .takeWhile(_ < height)
        .find(j2 => j2 == (height - 1) || grid(j2)(i) >= cur)
        .map(_ - j)
        .getOrElse(0)
      val left = Iterator
        .iterate(i - 1)(_ - 1)
        .takeWhile(_ >= 0)
        .find(i2 => i2 == 0 || grid(j)(i2) >= cur)
        .map(i - _)
        .getOrElse(0)
      val right = Iterator
        .iterate(i + 1)(_ + 1)
        .takeWhile(_ < width)
        .find(i2 => i2 == width - 1 || grid(j)(i2) >= cur)
        .map(_ - i)
        .getOrElse(0)
      val score = up * down * right * left
      if (score > max) max = score
    }
    max
  }
}

case class Vector(x: Int, y: Int) {
  def diff(other: Vector): Vector = {
    Vector(x - other.x, y - other.y)
  }
  def add(dir: Vector): Vector = {
    Vector(x + dir.x, y + dir.y)
  }
  def unitize: Vector = {
    Vector(x.max(-1).min(1), y.max(-1).min(1))
  }
  def max(v: Vector): Vector = {
    Vector(math.max(x, v.x), math.max(y, v.y))
  }
  def manhattanDistance(v: Vector): Int = {
    (x - v.x).abs + (y - v.y).abs
  }
  def divide(d: Int): Vector = {
    Vector(x / d, y / d)
  }
}
object Vector {
  val zero = Vector(0, 0)

  val up = Vector(0, 1)
  val down = Vector(0, -1)
  val right = Vector(1, 0)
  val left = Vector(-1, 0)
}

object day9a extends Puzzle {
  override val tests = Seq(
    (
      """R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
""",
      13
    )
  )

  def solve(lines: Iterator[String]): Int = {
    val positions = mutable.Set(Vector.zero)
    var head = Vector.zero
    var tail = Vector.zero
    lines.foreach { line =>
      val Array(h, t) = line.split(" ")
      val dir = h match {
        case "U" => Vector.up
        case "D" => Vector.down
        case "L" => Vector.left
        case "R" => Vector.right
        case _   => ???
      }
      (0 until t.toInt).foreach { _ =>
        head = head.add(dir)
        val diff = head.diff(tail)
        if (diff.x.abs > 1 || diff.y.abs > 1) {
          tail = tail.add(diff.unitize)
          positions.add(tail)
        }
      }
    }
    positions.size
  }
}

object day9b extends Puzzle {
  override def tests: Seq[(String, Any)] = Seq(
    (
      """R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20""",
      36
    )
  )
  override def solve(lines: Iterator[String]): Int = {
    val positions = mutable.Set(Vector.zero)
    val knots = Array.fill(10)(Vector.zero)
    lines.foreach { line =>
      val Array(h, t) = line.split(" ")
      val dir = h match {
        case "U" => Vector.up
        case "D" => Vector.down
        case "L" => Vector.left
        case "R" => Vector.right
        case _   => ???
      }
      (0 until t.toInt).foreach { _ =>
        knots(0) = knots(0).add(dir)
        for (x <- 1 until 10) {
          val c = knots(x)
          val p = knots(x - 1)
          val diff = p.diff(c)
          if (diff.x.abs > 1 || diff.y.abs > 1) {
            knots(x) = c.add(diff.unitize)
          }
        }
        positions.add(knots.last)
      }
    }
    positions.size
  }
}

object day10a extends Puzzle {
  override val tests = Seq(
    (
      """addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop""",
      13140
    )
  )
  override def solve(lines: Iterator[String]): Int = {
    var x = 1
    var cycle = 1
    var result = 0
    val interestingCycles = Set(
      20, 60, 100, 140, 180, 220
    )
    def maybeRecord: Unit = {
      if (interestingCycles.contains(cycle)) {
        result += x * cycle
      }
    }
    lines.map(_.split(" ")).foreach {
      case Array("addx", int) =>
        cycle += 1
        maybeRecord

        cycle += 1
        x += int.toInt
        maybeRecord
      case Array("noop") =>
        cycle += 1
        maybeRecord
    }

    result
  }
}

object day10b extends Puzzle {
  override def tests = day10a.tests.map { case (a, _) =>
    (
      a,
      """
##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######....."""
    )
  }

  override def solve(lines: Iterator[String]): Any = {
    // sprite x position
    var x = 1
    var cycle = 1
    var result = StringBuilder()
    // x position of pixel being drawn
    def pixelX: Int = (cycle - 1) % 40
    def maybeRecord: Unit = {
      val diff = pixelX - x
      if (diff >= -1 && diff <= 1) {
        result.append("#")
      } else {
        result.append(".")
      }

      // println(s"[$cycle] pix: $pixelX sprite: $x\n$result")
    }
    lines.map(_.split(" ")).foreach {
      case Array("addx", int) =>
        maybeRecord
        cycle += 1
        maybeRecord
        cycle += 1
        x += int.toInt
      case Array("noop") =>
        maybeRecord

        cycle += 1
    }

    "\n" + result.toString.grouped(40).mkString("\n")
  }
}

object day11a extends Puzzle {
  type MonkeyBusiness = BigInt

  override def tests: Seq[(String, MonkeyBusiness)] = Seq(
    (
      """Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
""",
      10605
    )
  )

  enum Expression {
    case Literal(x: Long)
    case Old
    case Add(left: Expression, right: Expression)
    case Mult(left: Expression, right: Expression)
    case Sub(left: Expression, right: Expression)
    case Div(left: Expression, right: Expression)
  }

  def parseOperand(str: String): Expression = {
    str match {
      case "old" => Expression.Old
      case _     => Expression.Literal(str.toInt)
    }
  }

  def parseOp(str: String): Expression = {
    str.split(" ") match {
      case Array(left, op, right) =>
        val l = parseOperand(left)
        val r = parseOperand(right)
        op match {
          case "+" => Expression.Add(l, r)
          case "-" => Expression.Sub(l, r)
          case "*" => Expression.Mult(l, r)
          case "/" => Expression.Div(l, r)
          case x   => throw new RuntimeException(s"unknown op '$x'")
        }
      case other =>
        throw new RuntimeException(s"unknown op str: ${other.toSeq}")
    }
  }

  case class OpContext(old: Long) {
    def execute(op: Expression): Long = {
      op match {
        case Expression.Literal(x) => x
        case Expression.Old        => old
        case Expression.Add(l, r)  => execute(l) + execute(r)
        case Expression.Sub(l, r)  => execute(l) - execute(r)
        case Expression.Mult(l, r) => execute(l) * execute(r)
        case Expression.Div(l, r)  => execute(l) / execute(r)
      }
    }
  }

  case class Monkey(
      items: ArrayDeque[Long],
      operation: Expression,
      test: Long,
      trueIdx: Int,
      falseIdx: Int
  ) {
    var numInspections = BigInt(0)
  }

  def parseMonkey(input: Iterator[String]): Monkey = {
    var line = input.next()
    val startingItems =
      line.dropWhile(!_.isDigit).split(", ").map(_.trim.toLong)

    line = input.next()
    val op = parseOp(line.dropWhile(_ != '=').drop(1).trim)
    line = input.next()
    val test = line.dropWhile(!_.isDigit).toInt
    line = input.next()
    val trueIdx = line.dropWhile(!_.isDigit).toInt
    line = input.next()
    val falseIdx = line.dropWhile(!_.isDigit).toInt
    Monkey(
      items = new ArrayDeque(startingItems.toSeq.asJava),
      operation = op,
      test = test,
      trueIdx = trueIdx,
      falseIdx = falseIdx
    )
  }

  override def solve(lines: Iterator[String]): MonkeyBusiness = {
    // parse Monkeys
    val monkeys = lines.flatMap { line =>
      if (line.startsWith("Monkey")) {
        Some(parseMonkey(lines))
      } else {
        None
      }
    }.toArray

    // println(s" monkeys: \n\n${monkeys.mkString("\n")}")

    // run 20 rounds (tracking item inspect count)
    (0 until 20).foreach { round =>
      monkeys.foreach { m =>
        while (!m.items.isEmpty()) {
          m.numInspections += 1
          var item = m.items.removeFirst()
          item = OpContext(item).execute(m.operation)
          item = item / 3
          val idx = if (item % m.test == 0) {
            m.trueIdx
          } else {
            m.falseIdx
          }
          monkeys(idx).items.addLast(item)
        }
      }
    }
    // calculate monkey business
    val Array(m1, m2) = monkeys.sortBy(-_.numInspections).take(2)
    m1.numInspections * m2.numInspections
  }
}

object day11b extends Puzzle {
  import day11a._
  override def tests = day11a.tests.map { case (a, _) => (a, 2713310158L) }

  override def solve(lines: Iterator[String]): MonkeyBusiness = {
    // parse Monkeys
    val monkeys = lines.flatMap { line =>
      if (line.startsWith("Monkey")) {
        Some(parseMonkey(lines))
      } else {
        None
      }
    }.toArray

    val totalTest = monkeys.map(_.test).reduce(_ * _)

    // println(s" monkeys: \n\n${monkeys.mkString("\n")}\n\n total test: $totalTest")

    // run 20 rounds (tracking item inspect count)
    (0 until 10000).foreach { round =>
      monkeys.foreach { m =>
        while (!m.items.isEmpty()) {
          m.numInspections += 1
          var item = m.items.removeFirst()
          item = OpContext(item).execute(m.operation)
          item = item % totalTest
          val idx = if (item % m.test == 0) {
            m.trueIdx
          } else {
            m.falseIdx
          }
          monkeys(idx).items.addLast(item)
        }
      }
    }
    // calculate monkey business
    val Array(m1, m2) = monkeys.sortBy(-_.numInspections).take(2)
    m1.numInspections * m2.numInspections
  }
}

object day12a extends Puzzle {
  override def tests: Seq[(String, Int)] = Seq(
    (
      """Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi""",
      31
    )
  )

  override def solve(lines: Iterator[String]): Int = {
    val map = lines.filter(_.nonEmpty).toArray
    val height = map.head.length
    val width = map.length

    def heightAt(v: Vector): Int = {
      map(v.x)(v.y) match {
        case 'E' => 'z'
        case 'S' => 'a'
        case x   => x
      }
    }

    var current =
      (0 until height)
        .flatMap { y => (0 until width).map(Vector(_, y)) }
        .find { v => map(v.x)(v.y) == 'S' }
        .get

    var target =
      (0 until height)
        .flatMap { y => (0 until width).map(Vector(_, y)) }
        .find { v => map(v.x)(v.y) == 'E' }
        .get

    val distances = mutable.Map[Vector, Int]()
    distances(current) = 0

    def getNeighbors(p: Vector): Seq[Vector] = {
      val cur = heightAt(p)
      val result = Seq(
        Vector(p.x, p.y + 1),
        Vector(p.x, p.y - 1),
        Vector(p.x + 1, p.y),
        Vector(p.x - 1, p.y)
      )
        .filter { v =>
          v.x >= 0 && v.x < width && v.y >= 0 && v.y < height
        }
        .filter { v =>
          heightAt(v) <= cur + 1
        }

      result
    }

    val nextPos = mutable.ArrayDeque[Vector](current)

    while (nextPos.nonEmpty) {
      val cur = nextPos.removeHead()
      val dist = distances(cur)
      if (cur == target) {
        return dist
      }
      getNeighbors(cur)
        .filterNot(distances.isDefinedAt)
        .foreach { n =>
          distances(n) = dist + 1
          nextPos.append(n)
        }
    }

    0
  }
}

object day12b extends Puzzle {
  override def tests: Seq[(String, Int)] = Seq(
    (
      """Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi""",
      29
    )
  )

  override def solve(lines: Iterator[String]): Int = {
    val map = lines.filter(_.nonEmpty).toArray
    val height = map.head.length
    val width = map.length

    def heightAt(v: Vector): Int = {
      map(v.x)(v.y) match {
        case 'E' => 'z'
        case 'S' => 'a'
        case x   => x
      }
    }

    val positions =
      (0 until height)
        .flatMap { y => (0 until width).map(Vector(_, y)) }

    var current =
      positions.find { v => map(v.x)(v.y) == 'S' }.get

    var target =
      positions.find { v => map(v.x)(v.y) == 'E' }.get

    def getNeighbors(p: Vector): Seq[Vector] = {
      val cur = heightAt(p)
      val result = Seq(
        Vector(p.x, p.y + 1),
        Vector(p.x, p.y - 1),
        Vector(p.x + 1, p.y),
        Vector(p.x - 1, p.y)
      )
        .filter { v =>
          v.x >= 0 && v.x < width && v.y >= 0 && v.y < height
        }
        .filter { v =>
          heightAt(v) <= cur + 1
        }

      result
    }

    var shortestDistance = Integer.MAX_VALUE

    positions
      .filter(v => heightAt(v) == 'a')
      .foreach { start =>
        current = start

      val distances = mutable.Map[Vector, Int]()
      distances(current) = 0

      val nextPos = mutable.ArrayDeque[Vector](current)

      while (nextPos.nonEmpty) {
        val cur = nextPos.removeHead()
        val dist = distances(cur)
        val nextDist = dist + 1
        if (cur == target) {
          if (dist < shortestDistance) {
            shortestDistance = dist
          }
          nextPos.clear()
        } else {
          getNeighbors(cur)
            .filter { n =>
              distances.get(n) match {
                case None    => true // valid neighbor
                case Some(d) => nextDist < d
              }
            }
            .foreach { n =>
              distances(n) = dist + 1
              nextPos.append(n)
            }
        }
      }
      }

    shortestDistance
  }
}

enum Packet {
  case Integer(i: Int)
  case List(l: Seq[Packet])
}
object Packet extends Ordering[Packet] {
  def toString(p: Packet): String = p match {
    case Packet.Integer(i) => i.toString
    case Packet.List(l)    => "[" + l.map(toString).mkString(",") + "]"
  }
  def compare(l: Packet, r: Packet): Int = {
    (l, r) match {
      case (Packet.Integer(l), Packet.Integer(r)) =>
        if (l < r) -1
        else if (l == r) 0
        else 1
      case (Packet.List(l), Packet.List(r)) =>
        var i = 0
        val max = math.max(l.size, r.size)
        while (i < max) {
          (Try(l(i)), Try(r(i))) match {
            case (Success(l), Success(r)) =>
              val result = compare(l, r)
              if (result != 0) return result
            case (Success(_), _) => return 1
            case (_, Success(_)) =>
              return -1
          }
          i += 1
        }
        0
      case (Packet.List(_), _) =>
        compare(l, Packet.List(Seq(r)))
      case (_, Packet.List(_)) =>
        compare(Packet.List(Seq(l)), r)
    }
  }

  class Parser(str: String) {
    var idx = 0
    private def consume(): Char = {
      idx += 1
      str(idx - 1)
    }

    private def peek(): Option[Char] = Try(str(idx)).toOption

    def readPacket(): Packet = {
      consume() match {
        case '[' => // list
          val buf = mutable.ListBuffer[Packet]()
          while (peek() != Some(']')) {
            buf.append(readPacket())
            peek() match {
              case Some(',') => consume()
              case _         =>
            }
          }
          assert(peek() == Some(']'))
          consume()
          Packet.List(buf.toSeq)
        case c if c.isDigit =>
          var value: Int = c - '0'
          while (peek().exists(_.isDigit)) {
            value *= 10
            value += consume() - '0'
          }
          Packet.Integer(value)
      }
    }
  }

  def parse(str: String): Packet = {
    Parser(str).readPacket()
  }
}

object day13a extends Puzzle {
  override def tests: Seq[(String, Int)] = Seq(
    (
      """[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]""",
      13
    )
  )

  override def solve(lines: Iterator[String]): Int = {
    lines
      .grouped(3)
      .map(_.take(2))
      .map(_.map(Packet.parse))
      .zipWithIndex
      .map { case (Seq(left, right), idx) =>
        val result = if (Packet.compare(left, right) <= 0) { idx + 1 }
        else { 0 }
        // println(Packet.toString(left))
        // println(Packet.toString(right))
        // println(result)
        // println()
        result
      }
      .sum
  }
}

object day13b extends Puzzle {
  override def tests = day13a.tests.map { case (a, _) => (a, 140) }
  override def solve(lines: Iterator[String]): Int = {
    val dividerPackets = Seq(
      Packet.parse("[[2]]"),
      Packet.parse("[[6]]")
    )
    val allPackets = lines
      .filter(_.nonEmpty)
      .map(Packet.parse)
      .toSeq
      ++ dividerPackets

    val sorted = allPackets.sorted(Packet)

    dividerPackets.map(sorted.indexOf).map(_ + 1).reduce(_ * _)
  }
}

class Grid[T](default: => T) {
  val map = mutable.Map[Vector, T]()
  var max = Vector(0, 0)
  def get(v: Vector): T = {
    map.getOrElse(v, default)
  }
  def set(v: Vector, t: T): Unit = {
    max = max.max(v)
    map.put(v, t)
  }
}

object day14a extends Puzzle {
  override def tests: Seq[(String, Any)] = Seq(
    (
      """498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
""",
      24
    )
  )

  override def solve(lines: Iterator[String]): Int = {
    val surfaces = lines.map { line =>
      line.split("->").map(_.trim).map { s =>
        val Array(x, y) = s.trim.split(",")
        Vector(x.toInt, y.toInt)
      }
    }
    val grid = new Grid('.')
    surfaces.foreach { points =>
      points.sliding(2, 1).foreach { case Array(a, b) =>
        val line = if (a.x == b.x) {
          (if (a.y < b.y) {
             a.y to b.y
           } else {
             b.y to a.y
           }).map(Vector(a.x, _))
        } else {
          (if (a.x < b.x) {
             a.x to b.x
           } else {
             b.x to a.x
           })
            .map(Vector(_, a.y))
        }
        line.foreach(grid.set(_, '#'))
      }
    }
    var abyss = false
    var numSands = 0
    while (abyss == false) {
      // simulate one sand falling
      // start
      var c = Vector(500, 0)
      var falling = true
      while (abyss == false && falling == true) {
        val success = Iterator(Vector(0, 1), Vector(-1, 1), Vector(1, 1))
          .map { dir =>
            val next = c.add(dir)
            grid.get(next) match {
              case '.' => // continue falling
                c = next
                true
              case _ => false
            }
          }
          .find(identity)
        if (success.isEmpty) {
          grid.set(c, 'o')
          falling = false
        } else {
          if (c.y >= grid.max.y) {
            abyss = true
          }
        }
      }

      if (abyss == false) {
        numSands += 1
      }
    }
    numSands
  }
}

object day14b extends Puzzle {
  override def tests: Seq[(String, Any)] = Seq(
    (
      """498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
""",
      93
    )
  )

  class MyGrid[T](default: => T) extends Grid[T](default) {
    var floor = 0
    var floorValue: T = _
    def setFloor(y: Int, value: T) = {
      floor = y
      floorValue = value
    }
    override def get(v: Vector): T = {
      if (v.y == floor) return floorValue
      super.get(v)
    }
  }

  override def solve(lines: Iterator[String]): Int = {
    val surfaces = lines.map { line =>
      line.split("->").map(_.trim).map { s =>
        val Array(x, y) = s.trim.split(",")
        Vector(x.toInt, y.toInt)
      }
    }
    val grid = new MyGrid('.')
    surfaces.foreach { points =>
      points.sliding(2, 1).foreach { case Array(a, b) =>
        val line = if (a.x == b.x) {
          (if (a.y < b.y) {
             a.y to b.y
           } else {
             b.y to a.y
           }).map(Vector(a.x, _))
        } else {
          (if (a.x < b.x) {
             a.x to b.x
           } else {
             b.x to a.x
           })
            .map(Vector(_, a.y))
        }
        line.foreach(grid.set(_, '#'))
      }
    }
    grid.setFloor(grid.max.y + 2, '#')
    var reachedTop = false
    var numSands = 0
    while (reachedTop == false) {
      // simulate one sand falling
      // start
      var c = Vector(500, -1)
      var falling = true
      while (falling == true) {
        val success = Iterator(Vector(0, 1), Vector(-1, 1), Vector(1, 1))
          .map { dir =>
            val next = c.add(dir)
            grid.get(next) match {
              case '.' => // continue falling
                c = next
                true
              case _ => false
            }
          }
          .find(identity)
        if (success.isEmpty) {
          grid.set(c, 'o')
          falling = false
        }
      }
      if (c == Vector(500, 0)) {
        reachedTop = true
      }
      numSands += 1
    }
    numSands
  }
}

case class Sensor(pos: Vector, closest: Vector) {
  val dist = pos.manhattanDistance(closest)
  def outline: Iterator[Vector] = {
    val d = dist + 1
    Iterator
      .iterate(Vector(pos.x + d, pos.y))(_.add(Vector(-1, 1)))
      .take(d)
    ++
    Iterator
      .iterate(Vector(pos.x - d, pos.y))(_.add(Vector(1, -1)))
      .take(d)
    ++
    Iterator
      .iterate(Vector(pos.x, pos.y + d))(_.add(Vector(-1, -1)))
      .take(d)
    ++
    Iterator
      .iterate(Vector(pos.x, pos.y - d))(_.add(Vector(1, 1)))
      .take(d)
  }
}

object day15a extends Puzzle {
  override def tests = Seq(
    (
      """row=10
Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3
""",
      26
    )
  )
  val SENSOR =
    """Sensor at x=(-?[0-9]*), y=(-?[0-9]*): closest beacon is at x=(-?[0-9]*), y=(-?[0-9]*)""".r
  val ROW = """row=(-?[0-9]*)""".r
  override def solve(lines: Iterator[String]): Int = {
    var row = 2000000
    val sensors = lines.flatMap {
      case SENSOR(x1, y1, x2, y2) =>
        Some(Sensor(Vector(x1.toInt, y1.toInt), Vector(x2.toInt, y2.toInt)))
      case ROW(r) =>
        row = r.toInt
        None
      case "" =>
        None
      case x => throw RuntimeException(s"failed at $x")
    }.toSeq

    val mid = sensors.map(_.pos).reduce(_.add(_)).divide(sensors.size)
    val maxDist = sensors.map(_.dist).max
    val max = sensors.map(_.pos).map(_.x).max + maxDist
    val min = sensors.map(_.pos).map(_.x).min - maxDist

    (min to max).iterator.map { x =>
      val v = Vector(x, row)
      if (
        !sensors.exists(s => s.closest == v || s.pos == v) &&
        sensors.exists { s =>
          s.pos.manhattanDistance(v) <= s.dist
        }
      ) {
        1
      } else {
        0
      }
    }.sum
  }
}

object day15b extends Puzzle {
  import day15a._
  override def tests = day15a.tests.map { case (a, b) => (a, 56000011) }
  override def solve(lines: Iterator[String]): Long = {
    var row = 2000000
    val sensors = lines.flatMap {
      case SENSOR(x1, y1, x2, y2) =>
        Some(Sensor(Vector(x1.toInt, y1.toInt), Vector(x2.toInt, y2.toInt)))
      case ROW(r) =>
        row = r.toInt
        None
      case "" =>
        None
      case x => throw RuntimeException(s"failed at $x")
    }.toSeq

    val limit = row * 2
    val beacon = sensors.iterator
      .flatMap(_.outline)
      .filter(v => v.x >= 0 && v.x <= limit && v.y >= 0 && v.y <= limit)
      .find { v =>
        !sensors.exists(s => s.pos.manhattanDistance(v) <= s.dist)
      }
      .get
    beacon.x * 4000000L + beacon.y
  }
}
