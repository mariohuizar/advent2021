package advent2021

import zio.console._

import scala.io.Source

object Day07 extends zio.App {
  private val readInput = zio.blocking.effectBlocking {
    val source = Source.fromFile("advent2021/src/resources/Day07Input.txt")
    try source.getLines().toList finally source.close()
  }

  def fuelCalculator(input: List[String]): Long = {
    val crabPositions = input.flatMap(_.split(",").toList).map(_.toInt)
    val fuelNeededForAllPositions = for (p <- crabPositions) yield crabPositions.map { e => Math.abs(e - p) }.sum
    fuelNeededForAllPositions.min
  }

  def run(args: List[String]) =
    readInput.map(fuelCalculator).flatMap(r => putStrLn(r.toString)).exitCode
}