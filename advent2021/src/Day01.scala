import zio._
import zio.console._

import scala.io.Source

object Day01 extends zio.App {
  private val readInput = zio.blocking.effectBlocking {
    val source = Source.fromFile("advent2021/src/resources/Day01Input.txt")
    try source.getLines().toList.map(_.toInt) finally source.close()
  }

  def find2021Increase(input: List[Int]): Int = {
    def loop(input: List[Int], acc: Int): Int = {
      input match {
        case x :: y :: xs =>
          if( x < y ) loop(y :: xs, acc + 1) else loop(y :: xs, acc)
        case _ => acc
      }
    }
    loop(input, 0)
  }

  def run(args: List[String]) =
    readInput.map(find2021Increase).flatMap(r => putStrLn(r.toString)).exitCode
}
