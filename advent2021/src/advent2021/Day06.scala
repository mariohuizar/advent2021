package advent2021

import zio.console._

import scala.annotation.tailrec
import scala.io.Source

object Day06 extends zio.App {
  private val readInput = zio.blocking.effectBlocking {
    val source = Source.fromFile("advent2021/src/resources/Day06Input.txt")
    try source.getLines().toList finally source.close()
  }

  def lanternFishCalculator(input: List[String]): Long = {
    val fish = input.flatMap(_.split(",").toList).map(_.toInt)

    @tailrec def reproduce(fish: List[Int], days: Int): List[Int] = {
      if (days == 0)
        fish
      else {
        val decreaseFishTimer = fish.map { e => if (e == 0) 6 else e - 1 }
        val newFish = List.fill(fish.count(_ == 0))(8)
        reproduce(decreaseFishTimer ::: newFish, days - 1)
      }
    }

    reproduce(fish = fish, days = 256).size
  }

  def run(args: List[String]) =
    readInput.map(lanternFishCalculator).flatMap(r => putStrLn(r.toString)).exitCode
}