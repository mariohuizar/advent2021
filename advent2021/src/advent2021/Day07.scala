package advent2021

import zio.console._

import scala.annotation.tailrec
import scala.io.Source

object Day07 extends zio.App {
  private val readInput = zio.blocking.effectBlocking {
    val source = Source.fromFile("advent2021/src/resources/Day07Input.txt")
    try source.getLines().toList.flatMap(_.split(",").toList).map(_.toInt) finally source.close()
  }

  def fuelCalculator(crabPositions: List[Int]): Long = {
    val fuelNeededForAllPositions = for (p <- 0 to crabPositions.size) yield crabPositions.map { e => Math.abs(e - p) }.sum
    fuelNeededForAllPositions.min
  }

  def fuelCalculatorPart2(crabPositions: List[Int]): Long = {
    @tailrec def stepCalculator(steps: Long, acc: Long): Long = {
      if (steps == 0) {
        acc
      } else {
        stepCalculator(steps - 1, acc + steps)
      }
    }

    val fuelNeededForAllPositions = for (p <- 0 to crabPositions.size)
      yield crabPositions.map { e => {
        stepCalculator(Math.abs(e - p), 0)
      }
      }.sum

    fuelNeededForAllPositions.min
  }

  def run(args: List[String]) =
    readInput.map(fuelCalculatorPart2).flatMap(r => putStrLn(r.toString)).exitCode
}