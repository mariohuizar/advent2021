package advent2021

import zio.console._

import scala.annotation.tailrec
import scala.io.Source

object Day04 extends zio.App {
  private val readInput = zio.blocking.effectBlocking {
    val source = Source.fromFile("advent2021/src/resources/Day04Input.txt")
    try source.getLines().toList finally source.close()
  }

  def bingoScore(input: List[String], winner: Boolean): Long = {
    val bingoNumbers = input.head.split(",").map(_.toInt).toList
    val boardNumbersAsString = input.drop(2).filter(_.nonEmpty)
    val boardNumbersAsListInts = boardNumbersAsString.map(_.replace("  ", " ").split(" ")).flatten.filter(_.nonEmpty).map(_.toInt)
    val sliceBoardToTuples = boardNumbersAsListInts.map(i => (i, false)).sliding(5, 5).toList
    val listOfBoards = sliceBoardToTuples.sliding(5, 5).toList

    // calculate how many numbers called needed to win with this board
    @tailrec def winnerPositionMarkedBoard(bingoNumbers: List[Int], board: List[List[(Int, Boolean)]], acc: Int): (Int, List[List[(Int, Boolean)]]) = {
      bingoNumbers match {
        case x :: xs =>
          val markedBoard = markNumber(board, x)
          if (checkIfWinner(markedBoard, x)) (acc, markedBoard) else {
            winnerPositionMarkedBoard(xs, markedBoard, acc + 1)
          }
        case Nil => (acc, markNumber(board, 0))
      }
    }

    val markedBoards = listOfBoards.map(b => winnerPositionMarkedBoard(bingoNumbers, b, 0))
    val (bingoNumberPosition, winnerBoardPosition) = if (winner) markedBoards.map(_._1).zipWithIndex.min else markedBoards.map(_._1).zipWithIndex.max
    val winnerMarkedBoard = markedBoards(winnerBoardPosition)._2

    val sumUnmarkedNumbers = winnerMarkedBoard.flatMap(_.map { e => if (e._2) 0 else e._1 }).sum
    sumUnmarkedNumbers * bingoNumbers(bingoNumberPosition)
  }

  // each board is represented as a List[List[(Int, Boolean)]],
  // where boolean represent if number has been called or not
  def markNumber(board: List[List[(Int, Boolean)]], calledNumber: Int): List[List[(Int, Boolean)]] = {
    board.map {
      _.map { e => if (e._1 == calledNumber) (e._1, true) else (e._1, e._2) }
    }
  }

  def checkIfWinner(board: List[List[(Int, Boolean)]], calledNumber: Int): Boolean = {
    board.exists { l => l.forall(e => e._2) } || board.transpose.exists { l => l.forall(e => e._2) }
  }

  // winner = true for part 1, winner = false for part 2
  def run(args: List[String]) =
    readInput.map(i => bingoScore(i, false)).flatMap(r => putStrLn(r.toString)).exitCode
}