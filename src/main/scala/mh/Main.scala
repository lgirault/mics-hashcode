package mh

import scala.io.Source

case class Ride(xStart: Int, yStart: Int, xFinsh: Int, yFinish: Int, earliestStart: Int, latestFinish: Int)
case class Input(numRows: Int, numCols: Int, numVehicles: Int, numRides: Int, perRideBonusOnStart: Int, numSteps: Int,
                 rides: Vector[Ride])

object Main {
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

  }
}