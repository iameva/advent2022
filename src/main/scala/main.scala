import scala.io.Source
import java.util.PriorityQueue
import java.util.ArrayDeque
import scala.collection.JavaConverters._
import scala.collection.mutable

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
    ),
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
    ("""$ cd /
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
95437)
  )

  val CD = "\\$ cd (.*)".r
  val LS = "$ ls"
  val DIR = "dir (.*)".r
  val FILE = "(\\d+) (.*)".r

  def solve(input: Iterator[String]): Int = {
    var currentDir = "/"
    def allParents = {
      Iterator.iterate(currentDir){ d => 
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
    ("""$ cd /
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
24933642)
  )

  val CD = "\\$ cd (.*)".r
  val LS = "$ ls"
  val DIR = "dir (.*)".r
  val FILE = "(\\d+) (.*)".r

  def solve(input: Iterator[String]): Int = {
    var currentDir = "/"
    def allParents = {
      Iterator.iterate(currentDir){ d => 
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
