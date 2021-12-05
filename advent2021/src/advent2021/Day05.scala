package advent2021

import zio.console._

import scala.annotation.tailrec
import scala.io.Source

object Day05 extends zio.App {
  private val readInput = zio.blocking.effectBlocking {
    val source = Source.fromFile("advent2021/src/resources/Day05Input.txt")
    try source.getLines().toList finally source.close()
  }

  def avoidDangerAreas(input: List[String]): Long = {
    val parsedInput: Seq[((Int, Int), (Int, Int))] = input.map {
      case s"$a,$b -> $c,$d" => ((a.toInt, b.toInt), (c.toInt, d.toInt))
    }

    val allPoints = parsedInput.flatMap {
      case (aPoint, bPoint) =>
        if (aPoint._1 == bPoint._1) {
          val (startPoint, endPoint) = if (aPoint._2 <= bPoint._2) (aPoint, bPoint) else (bPoint, aPoint)
          val elements = endPoint._2 - startPoint._2 + 1
          List.tabulate(elements)(n => (startPoint._1, startPoint._2 + n))
        } else if (aPoint._2 == bPoint._2) {
          val (startPoint, endPoint) = if (aPoint._1 <= bPoint._1) (aPoint, bPoint) else (bPoint, aPoint)
          val elements = endPoint._1 - startPoint._1 + 1
          List.tabulate(elements)(n => (startPoint._1 + n, startPoint._2))
        } else {
          List.empty
        }
    }

    allPoints.groupMapReduce(identity)(_ => 1)(_ + _).toList.count(e => e._2 > 1)
  }

  // winner = true for part 1, winner = false for part 2
  def run(args: List[String]) =
    readInput.map(avoidDangerAreas).flatMap(r => putStrLn(r.toString)).exitCode
}