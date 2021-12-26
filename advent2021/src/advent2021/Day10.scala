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

  @tailrec def findFirstIllegalCharacter(line: String, acc: List[Char], currentPos: Int): Option[Char] = {
    val pairs = Map('{' -> '}', '(' -> ')', '[' -> ']', '<' -> '>')
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

  @tailrec def getClosingCharacters(line: String, acc: List[Char], currentPos: Int): List[Char] = {
    val pairs = Map('{' -> '}', '(' -> ')', '[' -> ']', '<' -> '>')
    if (line.length == currentPos)
      acc.map { e => pairs.getOrElse(e, 'e') }
    else {
      val currentChar = line(currentPos)
      if (currentChar == '[' || currentChar == '(' || currentChar == '{' || currentChar == '<') {
        getClosingCharacters(line, currentChar :: acc, currentPos + 1)
      } else {
        if (pairs.getOrElse(acc.head, 'e').equals(currentChar))
          getClosingCharacters(line, acc.tail, currentPos + 1)
        else {
          acc.map { e => pairs.getOrElse(e, 'e') }
        }
      }
    }
  }

  def syntaxErrorScore(input: List[String]): Long = {
    val scores = Map('}' -> 1197, ')' -> 3, ']' -> 57, '>' -> 25137)

    input.flatMap {
      findFirstIllegalCharacter(_, List.empty, 0)
    }.map {
      scores.getOrElse(_, 0)
    }.sum
  }

  def incompleteLinesScore(input: List[String]): Long = {
    val points = Map('}' -> 3L, ')' -> 1L, ']' -> 2L, '>' -> 4L)
    // discard syntax error lines
    val incompleteLines = input
      .map { e => (findFirstIllegalCharacter(e, List.empty, 0), e) }
      .flatMap {
        case (None, e) => Some(e)
        case _ => None
      }

    val closingCharacters = incompleteLines.map {
      getClosingCharacters(_, List.empty, 0)
    }
    val scores = closingCharacters.map {
      _.map {
        points.getOrElse(_, 0L)
      }
    }.map { l =>
      l.foldLeft(0L) { (acc, e) =>
        acc * 5 + e
      }
    }

    val scoresSorted = scores.sorted
    scoresSorted((scores.length - 1) / 2)
  }

  def run(args: List[String]) =
    readInput.map(incompleteLinesScore).flatMap(r => putStrLn(r.toString)).exitCode
}
