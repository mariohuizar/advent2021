package advent2021

import zio.console._

import scala.annotation.tailrec
import scala.io.Source

object Day05 extends zio.App {
  private val readInput = zio.blocking.effectBlocking {
    val source = Source.fromFile("advent2021/src/resources/Day05Input.txt")
    try source.getLines().toList finally source.close()
  }

  def avoidDangerAreasPart2(input: List[String], vertical: Boolean): Long = {
    val parsedInput: Seq[((Int, Int), (Int, Int))] = input.map {
      case s"$a,$b -> $c,$d" => ((a.toInt, b.toInt), (c.toInt, d.toInt))
    }

    val sortedParsedInput = parsedInput.map {
      case ((a, b), (x, y)) =>
        if (a < x) {
          ((a, b), (x, y))
        } else if (a == x) {
          if (b <= y) ((a, b), (x, y)) else ((x, y), (a, b))
        } else {
          ((x, y), (a, b))
        }
    }

    val allPoints = sortedParsedInput.flatMap {
      case (startPoint, endPoint) =>
        if (startPoint._1 == endPoint._1) {
          val elements = endPoint._2 - startPoint._2 + 1
          List.tabulate(elements)(n => (startPoint._1, startPoint._2 + n))
        } else if (startPoint._2 == endPoint._2) {
          val elements = endPoint._1 - startPoint._1 + 1
          List.tabulate(elements)(n => (startPoint._1 + n, startPoint._2))
        } else if (startPoint._1 < endPoint._1 && startPoint._2 < endPoint._2) {
          if (vertical) {
            val elements = endPoint._1 - startPoint._1 + 1
            List.tabulate(elements)(n => (startPoint._1 + n, startPoint._2 + n))
          } else {
            List.empty
          }
        } else {
          if (vertical) {
            val elements = endPoint._1 - startPoint._1 + 1
            List.tabulate(elements)(n => (startPoint._1 + n, startPoint._2 - n))
          } else {
            List.empty
          }
        }
    }

    allPoints.groupMapReduce(identity)(_ => 1)(_ + _).toList.count(e => e._2 > 1)
  }

  def run(args: List[String]) =
    readInput.map(i => avoidDangerAreasPart2(i, true)).flatMap(r => putStrLn(r.toString)).exitCode
}