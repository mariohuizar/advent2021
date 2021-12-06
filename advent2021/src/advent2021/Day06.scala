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
    val fishCount = fish.groupMapReduce(identity)(_ => 1L)(_ + _)

    def reproduce(days: Int, fishCount: Map[Int, Long]): Map[Int, Long] = {
      if (days == 0) {
        fishCount
      }
      else {
        val decreaseFishTimer = fishCount.flatMap {
          case (0, c) => None // fish with timer 0 will get calculated separately
          case (t, c) => Some((t - 1, c))
        }
        val newFishCount = fishCount.get(0) match {
          case None => decreaseFishTimer
          case Some(kids) => {
            def incr(by: Long) = (c: Option[Long]) => Some(c.getOrElse(0L) + by)
            decreaseFishTimer.updatedWith(6)(incr(kids)).updatedWith(8)(incr(kids))
          }
        }
        reproduce(days - 1, newFishCount)
      }
    }

    reproduce(256, fishCount).view.values.sum
  }

  def run(args: List[String]) =
    readInput.map(lanternFishCalculator).flatMap(r => putStrLn(r.toString)).exitCode
}