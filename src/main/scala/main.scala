import scala.io.Source
import java.util.PriorityQueue
import scala.collection.JavaConverters._

trait Puzzle {
  def getDataFilePath: String =
    s"./data/${getClass.getSimpleName.reverse.dropWhile(!_.isDigit).reverse}"

  def tests: Seq[(String, Int)] = Nil

  def main(args: Array[String]): Unit = {
    tests.zipWithIndex.foreach { case ((input, expected), idx) =>
      val result = solve(
        input.trim.split("\n").map(_.trim).iterator
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

  def solve(lines: Iterator[String]): Int
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
