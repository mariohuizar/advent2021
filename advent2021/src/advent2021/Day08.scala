package advent2021

import zio.console._

import scala.annotation.tailrec
import scala.io.Source

object Day08 extends zio.App {
  private val readInput = zio.blocking.effectBlocking {
    val source = Source.fromFile("advent2021/src/resources/Day08Input.txt")
    try source.getLines().toList finally source.close()
  }

  def easyNumberCounter(input: List[String]): Long = {
    val digitOutputValues = input.map(_.split("\\|").toList.last).map(_.split(" ").toList.tail)
    val countDigitOutputValues = digitOutputValues.map( _.count(e => e.length == 3 || e.length == 4 || e.length == 2 || e.length == 7))
    countDigitOutputValues.sum
  }

  def completeDeduction(input: List[String]): Long = {
    val inp = input.map { e =>
      val patterns :: output :: Nil = e.split('|').toList
      (patterns.split(" ").toList, output.drop(1).split(" ").toList)
    }
    val output: List[Int] = {
      inp.map { case (patterns, output) =>
        def findByLength(l: Int): Set[Char] =
          patterns.find(_.length == l).map(_.toSet).get

        val `1` = findByLength(2)
        val `4` = findByLength(4)

        val key = patterns.map {
          case s if s.length == 5 =>
            s.toSet match {
              case chars if chars.intersect(`1`).size == 2 => chars -> 3
              case chars if chars.intersect(`4`).size == 2 => chars -> 2
              case chars                                   => chars -> 5
            }
          case s if s.length == 6 =>
            s.toSet match {
              case chars if chars.intersect(`1`).size == 1 => chars -> 6
              case chars if chars.intersect(`4`).size == 4 => chars -> 9
              case chars                                   => chars -> 0
            }
          case s =>
            s.length match {
              case 2 => s.toSet -> 1
              case 3 => s.toSet -> 7
              case 4 => s.toSet -> 4
              case 7 => s.toSet -> 8
            }
        }.toMap

        output.map(o => key(o.toSet)).mkString.toInt
      }
    }
    output.sum
  }

  def run(args: List[String]) =
    readInput.map(completeDeduction).flatMap(r => putStrLn(r.toString)).exitCode
}