import scala.collection.mutable

object day16a extends Puzzle {
  val VALVE_REGEX =
    """Valve (\w+) has flow rate=([0-9]+); tunnels? leads? to valves? (.*)""".r

  override def tests: Seq[(String, Any)] = Seq(
    (
      """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
       |Valve BB has flow rate=13; tunnels lead to valves CC, AA
       |Valve CC has flow rate=2; tunnels lead to valves DD, BB
       |Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
       |Valve EE has flow rate=3; tunnels lead to valves FF, DD
       |Valve FF has flow rate=0; tunnels lead to valves EE, GG
       |Valve GG has flow rate=0; tunnels lead to valves FF, HH
       |Valve HH has flow rate=22; tunnel leads to valve GG
       |Valve II has flow rate=0; tunnels lead to valves AA, JJ
       |Valve JJ has flow rate=21; tunnel leads to valve II""".stripMargin,
      1651
    )
  )

  case class State(
      timeLeft: Int = 30,
      released: Int = 0,
      openedValves: Set[String] = Set()
  )

  override def solve(lines: Iterator[String]): Int = {
    val rooms = mutable.Map[String, (Int, Set[String])]()
    lines.foreach { case VALVE_REGEX(valve, rate, edges) =>
      rooms.put(
        valve,
        (rate.toInt, edges.split(",").map(_.trim).toSet)
      )
    }

    def distToRoom(start: String, end: String): Int = {
      val queue = mutable.ArrayDeque[(List[String], Int)]((List(start), 0))
      while (queue.nonEmpty) {
        val (room :: rest, steps) = queue.removeHead()
        if (room == end) return steps
        rooms(room)._2.iterator
          .filterNot(rest.contains)
          .foreach { neighbor =>
            queue.append((neighbor :: room :: rest, steps + 1))
          }
      }
      return -1
    }

    val roomRoutes: Map[String, Map[String, Int]] = rooms.keySet.iterator.map {
      room =>
        // find cost to each other room
        val costs = rooms.keySet.iterator
          .filter(r => r != room && rooms(r)._1 > 0)
          .map { other =>
            (other, distToRoom(room, other))
          }
          .toMap
        room -> costs
    }.toMap
    // println(s"roomRoutes: ${roomRoutes.toSeq.sorted.mkString("\n")}")

    def findMax(node: String, state: State): Int = {
      if (state.timeLeft <= 0) return state.released

      val (thisRate, _) = rooms(node)
      val neighbors = roomRoutes(node)
        .filterKeys(k => !state.openedValves.contains(k))

      if (neighbors.nonEmpty) {
        neighbors.iterator.map { case (neighbor, time) =>
          val nextTime = state.timeLeft - time - 1
          val nextState = State(
            timeLeft = nextTime,
            released = state.released + rooms(neighbor)._1 * nextTime,
            openedValves = state.openedValves + neighbor
          )
          findMax(neighbor, nextState)
        }.max
      } else {
        state.released
      }
    }
    findMax("AA", State(30, 0, Set("AA")))
  }
}

object day16b extends Puzzle {
  val VALVE_REGEX =
    """Valve (\w+) has flow rate=([0-9]+); tunnels? leads? to valves? (.*)""".r

  override def tests: Seq[(String, Any)] = Seq(
    (
      """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
       |Valve BB has flow rate=13; tunnels lead to valves CC, AA
       |Valve CC has flow rate=2; tunnels lead to valves DD, BB
       |Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
       |Valve EE has flow rate=3; tunnels lead to valves FF, DD
       |Valve FF has flow rate=0; tunnels lead to valves EE, GG
       |Valve GG has flow rate=0; tunnels lead to valves FF, HH
       |Valve HH has flow rate=22; tunnel leads to valve GG
       |Valve II has flow rate=0; tunnels lead to valves AA, JJ
       |Valve JJ has flow rate=21; tunnel leads to valve II""".stripMargin,
      1707
    )
  )

  case class State(
      movers: Seq[(String, Int)],
      timeLeft: Int,
      released: Int,
      openedValves: Seq[String]
  )

  override def solve(lines: Iterator[String]): Int = {
    val rooms = mutable.Map[String, (Int, Set[String])]()
    lines.foreach { case VALVE_REGEX(valve, rate, edges) =>
      rooms.put(
        valve,
        (rate.toInt, edges.split(",").map(_.trim).toSet)
      )
    }

    def distToRoom(start: String, end: String): Int = {
      val queue = mutable.ArrayDeque[(List[String], Int)]((List(start), 0))
      while (queue.nonEmpty) {
        val (room :: rest, steps) = queue.removeHead()
        if (room == end) return steps
        rooms(room)._2.iterator
          .filterNot(rest.contains)
          .foreach { neighbor =>
            queue.append((neighbor :: room :: rest, steps + 1))
          }
      }
      throw new RuntimeException("WTF")
    }

    val roomRoutes: Map[String, Map[String, Int]] = rooms.keySet.iterator.map {
      room =>
        // find cost to each other room
        val costs = rooms.keySet.iterator
          .filter(r => r != room && rooms(r)._1 > 0)
          .map { other =>
            (other, distToRoom(room, other))
          }
          .toMap
        room -> costs
    }.toMap
    // println(s"roomRoutes: ${roomRoutes.toSeq.sorted.mkString("\n")}")

    val interestingValves = rooms.filter(_._2._1 > 0).size
    def factorial(n: Long): Long = if (n == 1) n else n * factorial(n - 1)
    println(
      s"num valves: ${interestingValves} combos: ${factorial(interestingValves.toLong)}"
    )

    def findMax(state: State): Int = {
      if (state.timeLeft <= 0) return state.released

      val moverIdx = state.movers
        .lastIndexWhere(_._2 == state.timeLeft)

      val mover = state.movers(moverIdx)._1

      val neighbors = roomRoutes(mover)
        .filter { case (k, time) =>
          ((state.timeLeft - time) > 1) &&
          !state.openedValves.contains(k)
        }

      if (neighbors.nonEmpty) {
        neighbors.iterator.map { case (neighbor, time) =>
          val nextTime = state.timeLeft - time - 1
          val nextMovers = state.movers.updated(moverIdx, neighbor -> nextTime)
          val nextState = State(
            movers = nextMovers,
            timeLeft = nextMovers.map(_._2).max,
            released = state.released + rooms(neighbor)._1 * nextTime,
            openedValves = state.openedValves :+ neighbor
          )
          findMax(nextState)
        }.max
      } else {
        // println(s"final! ${state.released}")
        state.released
      }
    }
    findMax(
      State(
        movers = Seq("AA" -> 26, "AA" -> 26),
        timeLeft = 26,
        released = 0,
        Seq("AA")
      )
    )
  }
}
