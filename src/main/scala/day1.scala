import scala.io.Source

trait Puzzle {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile(args.head)
    println(solve(input.getLines))
  }

  def solve(lines: Iterator[String]): Any
}

object day1a extends Puzzle {
  def solve(input: Iterator[String]): Any = {
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

    max
  }
}

object day1b extends Puzzle {
  import java.util.PriorityQueue
  import scala.collection.JavaConverters._
  def solve(input: Iterator[String]): Any = {
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

    queue.iterator().asScala.sum
  }
}
