package advent2021

import zio._
import zio.console._

import scala.annotation.tailrec
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

  def findFinalPositionPart2(input: List[String]): Long = {
    @tailrec def loop(input: List[String], aim: Long, horizontal: Long, depth: Long): Long = {
      input match {
        case x :: xs =>
          x match {
            case s"forward $f" => loop(xs, aim, horizontal + f.toLong, depth + aim*f.toLong)
            case s"down $d" => loop(xs, aim + d.toLong, horizontal, depth)
            case s"up $u" => loop(xs, aim - u.toLong, horizontal, depth)
            case _ => 0L
          }
        case _ => horizontal * depth
      }
    }

    loop(input = input, aim = 0, horizontal = 0, depth = 0)
  }

  def run(args: List[String]) =
    readInput.map(findFinalPositionPart2).flatMap(r => putStrLn(r.toString)).exitCode
}