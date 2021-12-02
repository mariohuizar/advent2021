package advent2021

import zio._
import zio.console._

import scala.io.Source

object Day02 extends zio.App {
  private val readInput = zio.blocking.effectBlocking {
    val source = Source.fromFile("advent2021/src/resources/Day02Input.txt")
    try source.getLines().toList finally source.close()
  }

  def findFinalPosition(input: List[String]): Long = {
    val horizontalPosition = input.map {
      case s"forward $f" => f.toLong
      case _ => 0
    }.sum
    val depth = input.map {
      case s"down $d" => d.toLong
      case s"up $u" => -u.toLong
      case _ => 0
    }.sum
    horizontalPosition * depth
  }

  def run(args: List[String]) =
    readInput.map(findFinalPosition).flatMap(r => putStrLn(r.toString)).exitCode
}