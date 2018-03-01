package mh

import java.net.URI

import scala.io.Source

case class Ride(xStart: Int, yStart: Int, xFinish: Int, yFinish: Int, earliestStart: Int, latestFinish: Int){
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
      |${rides.mkString("\n")}
      |""".stripMargin

  lazy  val ridesSet: Set[Ride] = rides.toSet

def getNeighbors(nth: Int = 0,
                 visited: Set[Ride] = Set.empty,
                 currentX: Int = 0,
                 currentY: Int = 0,
                 currentTime: Int = 0) : Set[Node] =
  if(nth >= numRides) Set.empty
  else {
    for {
      neighbor <- ridesSet -- visited
    } yield {
      val newVisited = visited + neighbor
      val goTo = Utils.distTime(currentX, currentY, neighbor.xStart, neighbor.yStart)
      val nextActualStart = Math.max(currentTime + goTo, neighbor.earliestStart)
      val nextActualEndTime = nextActualStart + neighbor.distTime
      Node(neighbor,
           nth+1,
           getNeighbors(nth+1, newVisited, neighbor.xFinish, neighbor.yFinish, nextActualEndTime),
            nextActualStart,
            nextActualEndTime)
    }
  }
}

object Utils {

  def distTime(xStart: Int, yStart: Int, xFinish: Int, yFinish: Int) : Int =
    Math.abs(xFinish - xStart) + Math.abs(yFinish - yStart)

  def distTimeBetween(r1: Ride, r2: Ride) = distTime(r1.xFinish, r1.yFinish, r2.xStart, r2.yStart)
}

case class Node(ride: Ride, nth: Int, neighbours: Set[Node], actualStartTime: Int, actualEndTime: Int) {
  override def toString(): String = {
    s"""
       | $ride $nth $actualEndTime $actualEndTime
       |   $neighbours
     """.stripMargin
  }
}

object Main {

  def read(uri: URI) : Input = parse(Source.fromFile(uri, "ASCII"))

  def parse(source: Source) = {
    val bufIterator = source.getLines()

    val header = bufIterator.next().split(" ")

    val Array(numRows, numCols, numVehicles, numRides, perRideBonusOnStart, numSteps) = header

    val rides = bufIterator.foldLeft(Vector.empty[Ride]) {
      case (rs, line) =>
        val Array(xStart, yStart, xFinish, yFinish, earliestStart, latestFinish) = line.split(" ")
        rs :+ Ride(xStart.toInt, yStart.toInt, xFinish.toInt, yFinish.toInt, earliestStart.toInt, latestFinish.toInt)
    }

    Input(numRows.toInt, numCols.toInt, numVehicles.toInt, numRides.toInt, perRideBonusOnStart.toInt, numSteps.toInt, rides)
  }



  def main(args: Array[String]) : Unit = {
    val exUrl = getClass.getClassLoader.getResource("a_example.in")
    val input = read(exUrl.toURI)

    println(input.getNeighbors().mkString("\n"))
  }
}
