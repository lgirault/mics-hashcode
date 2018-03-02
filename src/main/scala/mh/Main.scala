package mh

import java.net.URI

import scala.io.Source

case class Ride(id: Int, xStart: Int, yStart: Int, xFinish: Int, yFinish: Int, earliestStart: Int, latestFinish: Int){
  def distTime = Utils.distTime(xStart, yStart, xFinish, yFinish)

  override def toString: String = s"[tc = ${distTime}, start = ${earliestStart}, finish = ${latestFinish} ]"
}
case class Input(numRows: Int, numCols: Int, numVehicles: Int, numRides: Int, perRideBonusOnStart: Int, numSteps: Int,
                 rides: Vector[Ride]){
  override def toString: String =
    s"""${numVehicles} vehicles
      |${numRides} rides
      |${perRideBonusOnStart} perRideBonusOnStart
      |${numSteps} steps
      |
      |""".stripMargin

  lazy  val ridesSet: Set[Ride] = rides.toSet

def getNeighbors(nth: Int = 0,
                 visited: Set[Ride] = Set.empty,
                 currentX: Int = 0,
                 currentY: Int = 0,
                 currentTime: Int = 0)(implicit input: Input) : Set[Node] =
  if(nth >= numRides) Set.empty
  else {
    for {
      neighbor <- ridesSet -- visited
    } yield {
      val newVisited = visited + neighbor
      val goTo = Utils.distTime(currentX, currentY, neighbor.xStart, neighbor.yStart)
      val nextActualStart = Math.max(currentTime + goTo, neighbor.earliestStart)
      val nextActualEndTime = nextActualStart + neighbor.distTime

      println(neighbor)
      val neighbors: Set[Node] =
        if (neighbor.latestFinish <= nextActualEndTime && neighbor.earliestStart >= nextActualStart)
          getNeighbors(nth+1, newVisited, neighbor.xFinish, neighbor.yFinish, nextActualEndTime)
        else
          Set.empty

      val tempNode = Node(neighbor,
                          nth+1,
                          neighbors,
                          nextActualStart,
                          nextActualEndTime, 0)

      val maxNeighborScore = if (neighbors.isEmpty) 0 else neighbors.maxBy(_.score).score
      tempNode.copy(score = Node.evaluateNeighbor(tempNode) + maxNeighborScore)
    }
  }
}

object Utils {

  def distTime(xStart: Int, yStart: Int, xFinish: Int, yFinish: Int) : Int =
    Math.abs(xFinish - xStart) + Math.abs(yFinish - yStart)

  def distTimeBetween(r1: Ride, r2: Ride) = distTime(r1.xFinish, r1.yFinish, r2.xStart, r2.yStart)
}

object Node {
  def evaluateNeighbor(neighbor: Node)(implicit input: Input): Int = {
    val ride = neighbor.ride
    val bonus =
      if (neighbor.actualStartTime <= ride.earliestStart)
        input.perRideBonusOnStart
      else
        0

    val rideValue = ride.distTime
    if (neighbor.actualEndTime <= ride.latestFinish)
      (bonus + rideValue)
    else 0
  }
}

case class Node(ride: Ride, nth: Int, neighbours: Set[Node], actualStartTime: Int, actualEndTime: Int, score: Int) {
  override def toString(): String = {
    s"""
       | $ride $nth $actualEndTime $actualEndTime
       |   $neighbours
     """.stripMargin
  }
}

object Dijkstra {
  def maxPath(graph : Set[Node], result: List[Node] = Nil, finished: Set[Int] = Set.empty): List[Node] = {
    val filtered = graph.filterNot(n => finished.contains(n.ride.id))

    if(filtered.isEmpty) result.reverse
    else {
      val bestNeighbor = filtered.maxBy(_.score)
      maxPath(bestNeighbor.neighbours, bestNeighbor :: result)
    }
  }
}

object Main {

  def read(uri: URI) : Input = parse(Source.fromFile(uri, "ASCII"))

  def parse(source: Source) = {
    val bufIterator = source.getLines()

    val header = bufIterator.next().split(" ")

    val Array(numRows, numCols, numVehicles, numRides, perRideBonusOnStart, numSteps) = header

    val rides = bufIterator.foldLeft((Vector.empty[Ride], 0)) {
      case ((rs, i), line) =>
        val Array(xStart, yStart, xFinish, yFinish, earliestStart, latestFinish) = line.split(" ")
        (rs :+ Ride(i, xStart.toInt, yStart.toInt, xFinish.toInt, yFinish.toInt, earliestStart.toInt, latestFinish.toInt), i + 1)
    }._1

    Input(numRows.toInt, numCols.toInt, numVehicles.toInt, numRides.toInt, perRideBonusOnStart.toInt, numSteps.toInt, rides)
  }



  def main(args: Array[String]) : Unit = {

    val exUrl = getClass.getClassLoader.getResource("a_example.in")
    implicit val input = read(exUrl.toURI)
    println(input)
    val graph = input.getNeighbors()
   // println(input.getNeighbors().mkString("\n"))
    val output = Range(0, input.numVehicles).foldLeft((Set.empty[Int], List.empty[List[Int]])){
      case ((visited, acc), _) =>
        val path = Dijkstra.maxPath(graph, Nil, visited).map(_.ride.id)
        (visited ++ path, path::acc)
    }._2
    println(s"${output.map(rides => rides.length + " " + rides.mkString(" ")).mkString("\n")}")
  }
}
