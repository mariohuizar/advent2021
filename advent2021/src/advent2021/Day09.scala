package advent2021

import zio.console._

import scala.io.Source

object Day09 extends zio.App {
  private val readInput = zio.blocking.effectBlocking {
    val source = Source.fromFile("advent2021/src/resources/Day09Input.txt")
    try source.getLines().toList finally source.close()
  }

  final case class Node(height: Int, up: Option[Int], down: Option[Int], left: Option[Int], right: Option[Int])

  def findLowPoints(input: List[String]): Long = {
    val inputAsInts = input.map(_.toList.map(_.asDigit))

    // transform three adjacent line of ints to a single node list
    def getNodeLine(line: List[Option[Int]], upLine: List[Option[Int]], downLine: List[Option[Int]]): List[Node] = {
      val leftRightNeighbors = line.sliding(3).map {
        case l :: Some(e) :: r :: Nil => Node(height = e, up = None, down = None, left = l, right = r)
      }.toList
      upLine.zip(downLine).tail.zip(leftRightNeighbors).map { e => e._2.copy(up = e._1._1, down = e._1._2) }
    }

    // add None elements at start and end to do a sliding(3, 1) through all data
    val inputAsSomeInts = inputAsInts.map(_.map(Some(_))).map { l => (None :: l) :+ None }
    val emptyIntList = List.fill(inputAsSomeInts.head.size)(None)
    val inputWithPadding = (emptyIntList :: inputAsSomeInts) :+ emptyIntList
    val nodes = inputWithPadding.sliding(3, 1).toList.map { t => getNodeLine(t(1), t.head, t(2)) }

    val lowPoints = nodes.flatten.filter( { n: Node =>
        (n.height < n.up.getOrElse(10)) && (n.height < n.down.getOrElse(10)) &&
          (n.height < n.left.getOrElse(10)) && (n.height < n.right.getOrElse(10))
    } )

    lowPoints.map{ e => 1 + e.height}.sum
  }

  def run(args: List[String]) =
    readInput.map(findLowPoints).flatMap(r => putStrLn(r.toString)).exitCode
}