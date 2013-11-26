package clashcode.robot

import scala.util.Random
import scala.collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: Cyrus
 * Date: 25.11.13
 * Time: 21:02
 * To change this template use File | Settings | File Templates.
 */
case class Field(fieldSize: Int, itemCount: Int, cells: Seq[Cell.Value]) {



}

class Game(field: Field, random: Random) {

  val cells = mutable.ArraySeq(field.cells : _*)
  var itemCount = field.itemCount
  var x = 1;
  var y = 1;

  /** get value of cell on position */
  private def cell(x: Int, y: Int) : Cell.Value = cells(y * field.fieldSize + x)

  def situation : Situation = {
    val canPickup = cell(x, y) == Cell.STUFF;
    Situation(cell(x, y - 1), cell(x + 1, y), cell(x, y + 1), cell(x - 1, y), canPickup)
  }

  def situationIndex : Int = Situations.getIndex(situation)

  /** pick up item if possible, return points */
  private def pickUp() : Int = {
    val index = y * field.fieldSize + x
    if (cells(index) == Cell.STUFF)
    {
      itemCount -= 1
      cells(index) = Cell.EMPTY
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

  private val fieldSize = 12;
  private val itemCount = 50;

  lazy private val testFields = (0 until 100).map(createRandomField)

  lazy private val possibleLocations = for {
    x <- 1 until fieldSize - 1
    y <- 1 until fieldSize - 1
  } yield (x + y * fieldSize)

  lazy private val wallLocations = (for {
    x <- 0 until fieldSize
    y <- 0 until fieldSize if (y == 0 || x == 0 || x == fieldSize - 1 || y == fieldSize - 1)
  } yield (x + y * fieldSize)).toSet

  println(Evaluator.testFields.forall(f => f.cells.count(c => c == Cell.STUFF) == 50))

  /** create deterministic random field from given random seed */
  private def createRandomField(seed: Int) = {
    val random = new Random(seed)
    val locations = random.shuffle(possibleLocations).take(itemCount).toSet
    val cells = (0 until fieldSize * fieldSize).map(index =>
      if (wallLocations.contains(index)) Cell.WALL
      else if (locations.contains(index)) Cell.STUFF
      else Cell.EMPTY
    )
    Field(fieldSize, itemCount, cells)
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
      for (i <- 0 until 200) {
        val index = game.situationIndex
        val decision = decisions(index)
        val p = game.act(decision)
        //println(decision + ": " + p)
        points += p
      }
    })
    points
  }

}
