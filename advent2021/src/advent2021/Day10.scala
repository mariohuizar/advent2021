package advent2021

import zio.console._

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.io.Source

object Day10 extends zio.App {
  private val readInput = zio.blocking.effectBlocking {
    val source = Source.fromFile("advent2021/src/resources/Day10Input.txt")
    try source.getLines().toList finally source.close()
  }

  def syntaxErrorScore(input: List[String]): Int = {
    val pairs = Map('{' -> '}', '(' -> ')', '[' -> ']', '<' -> '>')
    val scores = Map('}' -> 1197, ')' -> 3, ']' -> 57, '>' -> 25137)

    @tailrec def findFirstIllegalCharacter(line: String, acc: List[Char], currentPos: Int): Option[Char] = {
      if (line.length == currentPos)
        None
      else {
        val currentChar = line(currentPos)
        if (currentChar == '[' || currentChar == '(' || currentChar == '{' || currentChar == '<') {
          findFirstIllegalCharacter(line, currentChar :: acc, currentPos + 1)
        } else {
          if (pairs.getOrElse(acc.head, 'e').equals(currentChar))
            findFirstIllegalCharacter(line, acc.tail, currentPos + 1)
          else {
            Some(currentChar)
          }
        }
      }
    }

    input.flatMap {
      findFirstIllegalCharacter(_, List.empty, 0)
    }.map {
      scores.getOrElse(_, 0)
    }.sum
  }

  def run(args: List[String]) =
    readInput.map(syntaxErrorScore).flatMap(r => putStrLn(r.toString)).exitCode
}
