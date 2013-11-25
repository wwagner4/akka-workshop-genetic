package clashcode.robot

import scala.util.Random

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
      val n = (if (x(0)) 4 else 0) + (if (x(1)) 2 else 0) + (if (x(2)) 1 else 0)
      Decisions.all(n.min(5))
    })
    Candidate(decisions.toIndexedSeq)
  }
}

trait Decision

case class Move(x: Int, y: Int) extends Decision
case object MoveRandom extends Decision
case object Stay extends Decision
case object PickUp extends Decision

object Decisions {
  val all = IndexedSeq(
    Move(0, -1),
    Move(+1, 0),
    Move(0, +1),
    Move(-1, 0),
    PickUp,
    MoveRandom)
  val lookup = all.zipWithIndex.toMap
}


case class Candidate(decisions: IndexedSeq[Decision])

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
