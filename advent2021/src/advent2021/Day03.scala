package advent2021

import zio.console._

import scala.annotation.tailrec
import scala.io.Source

object Day03 extends zio.App {
  private val readInput = zio.blocking.effectBlocking {
    val source = Source.fromFile("advent2021/src/resources/Day03Input.txt")
    try source.getLines().toList finally source.close()
  }

  def binaryDiagnostic(input: List[String]): Long = {
    val totalItems = input.length
    val gammaRateAsListInt = input.map(_.map(_.asDigit)).transpose.map(_.sum).map(e => if (e >= totalItems/2) 1 else 0)
    val betaRateAsListInt = gammaRateAsListInt.map(e => if (e == 0) 1 else 0)
    def listIntToDecimal(li : List[Int]): Int = Integer.parseInt(li.map(_.toString).foldLeft("")((a,b) => a+b), 2)
    val gammaRate = listIntToDecimal(gammaRateAsListInt)
    val betaRate = listIntToDecimal(betaRateAsListInt)
    gammaRate * betaRate
  }

  def lifeSuportRating(originalInput: List[String]): Long = {
    @tailrec def calcRating(input: List[(String, Int)], filterMajority: Boolean): Long = {
      if (input.size == 1) {
        Integer.parseInt(originalInput(input.head._2), 2)
      } else {
        val size = input.size
        val ones = input.map(_._1.head.asDigit).sum
        val condition = if (filterMajority) { ones >= size-ones } else { ones < size-ones }
        val remainingList = if (condition) input.filter(_._1.startsWith("1"))  else input.filter(_._1.startsWith("0"))
        calcRating(remainingList.map(e => (e._1.tail, e._2)), filterMajority)
      }
    }
    def oxigenRating(input: List[(String, Int)]): Long = {
      calcRating(input, true)
    }
    def co2ScrubberRating(input: List[(String, Int)]): Long = {
      calcRating(input, false)
    }

    oxigenRating(originalInput.zipWithIndex) * co2ScrubberRating(originalInput.zipWithIndex)
  }

  def run(args: List[String]) =
    readInput.map(lifeSuportRating).flatMap(r => putStrLn(r.toString)).exitCode
}