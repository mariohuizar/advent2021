package advent2021

import zio.console._

import scala.io.Source

object Day09 extends zio.App {
  private val readInput = zio.blocking.effectBlocking {
    val source = Source.fromFile("advent2021/src/resources/Day09Input.txt")
    try source.getLines().toList finally source.close()
  }

  final case class Node(height: Int, up: Option[Int], down: Option[Int], left: Option[Int], right: Option[Int])

  def transformInputToNodes(input: List[String]): List[List[Node]] = {
    val inputAsInts = input.map(_.toList.map(_.asDigit))

    // transform three adjacent line of ints to a single node list
    def getNodeLine(line: List[Option[Int]], upLine: List[Option[Int]], downLine: List[Option[Int]]): List[Node] = {
      val leftRightNeighbors = line.sliding(3).map {
        case l :: Some(e) :: r :: Nil => Node(height = e, up = None, down = None, left = l, right = r)
      }.toList
      upLine.zip(downLine).tail.zip(leftRightNeighbors).map { e => e._2.copy(up = e._1._1, down = e._1._2) }
    }

    // add None elements around data to do a sliding(3, 1) through all
    val inputAsSomeInts = inputAsInts.map(_.map(Some(_))).map { l => (None :: l) :+ None }
    val emptyIntList = List.fill(inputAsSomeInts.head.size)(None)
    val inputWithPadding = (emptyIntList :: inputAsSomeInts) :+ emptyIntList
    inputWithPadding.sliding(3, 1).toList.map { t => getNodeLine(t(1), t.head, t(2)) }
  }

  def isLowPoint(n: Node): Boolean = {
    (n.height < n.up.getOrElse(10)) && (n.height < n.down.getOrElse(10)) &&
      (n.height < n.left.getOrElse(10)) && (n.height < n.right.getOrElse(10))
  }

  def part1(input: List[String]): Long = {
    val nodes = transformInputToNodes(input).flatten
    val lowPoints = nodes.filter(isLowPoint)

    lowPoints.map { e => 1 + e.height }.sum
  }

  def part2(input: List[String]): Long = {
    val nodes = transformInputToNodes(input).map(_.toVector).toVector
    val lowPointsWithCoordinates = for (col <- nodes.head.indices;
                                        row <- nodes.indices if isLowPoint(nodes(row)(col))
                                        ) yield (nodes(row)(col), (row, col))

    // breadth first search, visit should contain the low point when first called.
    def basinElements(visit: Set[(Int, Int)], seen: Set[(Int, Int)]): Set[(Int, Int)] = {
      if (visit.nonEmpty) {
        // for each point on visit, check adjacent nodes and add them to the search if < 9
        val locationsToGoNext = visit.map{ p =>
          val rightLocation =
            if (nodes(p._1)(p._2).right.getOrElse(9) < 9 && !seen.contains(p._1, p._2 + 1)) Some((p._1, p._2 + 1)) else None
          val leftLocation =
            if (nodes(p._1)(p._2).left.getOrElse(9) < 9 && !seen.contains(p._1, p._2 - 1)) Some((p._1, p._2 - 1)) else None
          val upLocation =
            if (nodes(p._1)(p._2).up.getOrElse(9) < 9 && !seen.contains(p._1 - 1, p._2)) Some((p._1 - 1, p._2)) else None
          val downLocation =
            if (nodes(p._1)(p._2).down.getOrElse(9) < 9 && !seen.contains(p._1 + 1, p._2)) Some((p._1 + 1, p._2)) else None
          Set(rightLocation, leftLocation, upLocation, downLocation).flatten
        }
        basinElements(locationsToGoNext.flatten, seen ++ locationsToGoNext.flatten)
      } else {
        seen
      }
    }

    val listOfBasins = lowPointsWithCoordinates.map{lp => basinElements(Set(lp._2), Set.empty)}.toList
    val sizeOfBasins = listOfBasins.map(b => b.size)
    sizeOfBasins.sorted.takeRight(3).product
  }

  def run(args: List[String]) =
    readInput.map(part2).flatMap(r => putStrLn(r.toString)).exitCode
}