package clashcode.robot

import scala.util.Random

object Cell extends Enumeration {
  val EMPTY = Value(0)
  val WALL = Value(1)
  val STUFF = Value(2)
}

/** the code that represents the decisions of the robot in all situations */
case class CandidateCode(bits: Array[Byte]) {
  if (bits.length != Situations.codeLength) throw new IllegalArgumentException("Length of bits must be " + Situations.codeLength)

  /** evaluate this code */
  def evaluate : CandidatePoints = {
    val decisions = toDecisions
    val points = Evaluator.evaluate(decisions)
    CandidatePoints(this, points)
  }

  /** get decisions from this code */
  private def toDecisions : IndexedSeq[Decision] = {
    bits.map(x => Decisions.all(x))
  }
}

trait Decision

case class Move(x: Int, y: Int) extends Decision
case object MoveRandom extends Decision
case object Stay extends Decision
case object PickUp extends Decision

object Decisions {
  val all : IndexedSeq[Decision] = IndexedSeq(
    Move(0, -1),
    Move(+1, 0),
    Move(0, +1),
    Move(-1, 0),
    PickUp,
    MoveRandom)
  val count = all.length
  val lookup = all.zipWithIndex.toMap
}

case class CandidatePoints(code: CandidateCode, points: Int)

object Situations {

  type Situation = Int

  val all : IndexedSeq[Situation] = {
    val result = for {
      center <- List(Cell.EMPTY, Cell.STUFF)
      topCell <- Cell.values
      rightCell <- Cell.values
      bottomCell <- Cell.values if (topCell != Cell.WALL || bottomCell != Cell.WALL)
      leftCell <- Cell.values if (rightCell != Cell.WALL || leftCell != Cell.WALL)
    } yield (getSituation(topCell, rightCell, bottomCell, leftCell, center))
    result.toIndexedSeq
  }

  val codeLength = all.length

  private val indexBySituation = (0 to all.max).map(situation => all.indexOf(situation)).toArray

  println(indexBySituation)

  def getSituation(top: Cell.Value, right: Cell.Value, bottom: Cell.Value, left: Cell.Value, center: Cell.Value) : Situation =
    (top.id * 3 * 3 * 3 * 3 + right.id * 3 * 3 * 3 + bottom.id * 3 * 3 + left.id * 3 + center.id)

  def getIndex(situation: Situation) : Int = indexBySituation(situation)

  def getRandomCode : CandidateCode = {
    CandidateCode(Array.fill(codeLength)(Random.nextInt(Decisions.count).toByte))
  }

}
