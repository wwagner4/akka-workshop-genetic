package clashcode.robot

import scala.util.Random

/**
 * Created with IntelliJ IDEA.
 * User: Christian
 * Date: 25.11.13
 * Time: 17:39
 * To change this template use File | Settings | File Templates.
 */

object Cell extends Enumeration {
  val EMPTY = Value(0)
  val WALL = Value(1)
  val STUFF = Value(2)
}

/** represents a situation of the robot with 4 directions and presence of stuff */
case class Situation(sides: IndexedSeq[Cell.Value], canPickup: Boolean) {
}

case class CandidateEntry(bits: Array[Boolean]) {
  if (bits.length != 384) throw new IllegalArgumentException("Length of bits must be 384")

  /** create candidate from this entry */
  def toCandidate : Candidate = {
    val decisions = bits.grouped(3).map(x => {
      val direction = (if (x(1)) 2 else 0) + (if (x(2)) 1 else 0)
      Decision(x(0), Directions.all(direction))
    })
    Candidate(decisions.toIndexedSeq)
  }
}

case class Direction(x: Int, y: Int)
object Directions {
  val all = IndexedSeq(Direction(0, -1), Direction(+1, 0), Direction(0, +1), Direction(-1, 0))
  val lookup = all.zipWithIndex.toMap
}

case class Decision(pickUp: Boolean, direction: Direction)

case class Candidate(decisions: IndexedSeq[Decision]) {


}

object Situations {

  lazy val all : IndexedSeq[Situation] = {
    val result = for {
      canPickup <- IndexedSeq(false, true)
      topCell <- Cell.values
      rightCell <- Cell.values
      bottomCell <- Cell.values if (topCell != Cell.WALL || bottomCell != Cell.WALL)
      leftCell <- Cell.values if (rightCell != Cell.WALL || leftCell != Cell.WALL)
    } yield (Situation(IndexedSeq(topCell, rightCell, bottomCell, leftCell), canPickup))
    result
  }

  lazy private val indices : Map[Situation, Int] = all.zipWithIndex.toMap

  def getIndex(situation: Situation) : Int = indices(situation)

  def getRandomEntry : CandidateEntry = {
    CandidateEntry(Array.fill(384)(Random.nextBoolean()))
  }

}
