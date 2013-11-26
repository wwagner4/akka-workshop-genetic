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

  val indexBySituation = (0 to all.max).map(situation => all.indexOf(situation)).toArray

  def getSituation(top: Cell.Value, right: Cell.Value, bottom: Cell.Value, left: Cell.Value, center: Cell.Value) : Situation =
    (top.id * 3 * 3 * 3 * 3 + right.id * 3 * 3 * 3 + bottom.id * 3 * 3 + left.id * 3 + center.id)

  def getIndex(situation: Situation) : Int = indexBySituation(situation)

  def getRandomCode : CandidateCode = {
    CandidateCode(Array.fill(codeLength)(Random.nextInt(Decisions.count).toByte))
  }


  def translateFromPaper() {
    //val code = "254355153256235251056355461151336154151034156110550150052030256256132252350325112052333054055231255051336154150665264150266506012264453605631520256431054354632404350334153250253251352352045150130156213436252353223135051260513356201524514343432"
    val code = "656353656252353252656353656151353151252353252151353151656353656252353252656353656050353050252353252050353050151353151252353252151353151050353050252353252050353050656353562523532526563536656151353151252353252151353151656353656252353252656353454"
    val decisions = code.zipWithIndex.map {
      case (c, i) => {
        def translate(i : Int) = if (i == 0) 0 else if (i == 1) 2 else 1
        val top = translate((i / (3 * 3 * 3 * 3)) % 3)
        val bottom = translate((i / (3 * 3 * 3)) % 3)
        val right = translate((i / (3 * 3)) % 3)
        val left = translate((i / (3)) % 3)
        val center = translate(i % 3)
        val situation = (top * 3 * 3 * 3 * 3 + right * 3 * 3 * 3 + bottom * 3 * 3 + left * 3 + center)

        val decision = c.toString.toInt
        val translatedDecision =
          if (decision == 0) 0
          else if (decision == 1) 2
          else if (decision == 2) 1
          else if (decision == 3) 3
          else if (decision == 4) 5 //stay put
          else if (decision == 5) 4
          else 5 // random

        (situation : Situation, translatedDecision)
      }
    }.toMap

    println(all.map(situation => decisions(situation)).mkString(""))
  }
}
