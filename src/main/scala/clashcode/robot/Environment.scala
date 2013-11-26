package clashcode.robot

import scala.util.Random
import scala.collection.mutable


case class Field(fieldSize: Int, itemCount: Int, items: Seq[Boolean])

class Game(field: Field, random: Random) {

  val items = mutable.ArraySeq(field.items : _*)
  var itemCount = field.itemCount
  var x = 0;
  var y = 0;

  /** get value of cell on position */
  private def cell(x: Int, y: Int) : Cell.Value = {
    if (y < 0 || x < 0 || x >= field.fieldSize || y >= field.fieldSize)
      Cell.WALL
    else if (items(y * field.fieldSize + x))
      Cell.STUFF
    else
      Cell.EMPTY
  }

  def situationIndex : Int = {
    Situations.getIndex(Situations.getSituation(cell(x, y - 1), cell(x + 1, y), cell(x, y + 1), cell(x - 1, y), cell(x, y)))
  }

  /** pick up item if possible, return points */
  private def pickUp() : Int = {
    val index = y * field.fieldSize + x
    if (items(index))
    {
      itemCount -= 1
      items(index) = false
      10 // success: gain points
    }
    else
      -1 // lost points
  }

  /** move robot if possible */
  private def move(dx: Int, dy: Int) : Int = {
    val nextX = x + dx
    val nextY = y + dy
    if (cell(nextX, nextY) != Cell.WALL)
    {
      x = nextX
      y = nextY
      0 // move successful
    }
    else
      -5 // lost points
  }

  /** act as robot, returns points gained */
  def act(decision: Decision) : Int = {
    decision match {
      case Move(dx, dy) => move(dx, dy)
      case MoveRandom =>
        val randomMove = Decisions.all(random.nextInt(4)).asInstanceOf[Move]
        move(randomMove.x, randomMove.y)
      case Stay => 0
      case PickUp => pickUp()
    }
  }

}

object Evaluator {

  private val fieldSize = 10;

  private val itemCount = fieldSize * fieldSize / 2; // 50%

  lazy private val testFields = (0 until 200).map(seed => createRandomField(new Random(seed)))

  /** create deterministic random field from given random seed */
  private def createRandomField(random: Random) = {
    var fieldItemCount = 0
    val items = Array.fill(fieldSize * fieldSize)(false)
    while (fieldItemCount < itemCount) {
      val index = random.nextInt(items.length)
      if (!items(index)) {
        items(index) = true
        fieldItemCount += 1
      }
    }
    Field(fieldSize, itemCount, items)
  }

  /** get points for given candidate
    * 20 trials on different fields, */
  def evaluate(decisions: IndexedSeq[Decision]) : Int = {
    val moveRandom = new Random(0)
    var points = 0

    // test on 20 fields
    testFields.foreach(testField => {
      val game = new Game(testField, moveRandom)

      // max 200 robot turns
      var turns = 0
      while (turns < 200 && game.itemCount > 0) {
        turns += 1
        val index = game.situationIndex
        val decision = decisions(index)
        val p = game.act(decision)
        points += p
      }

      //points += 200 - turns // extra points for completing early
    })
    points
  }

}
