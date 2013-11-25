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

class Game(field: Field) {

  val moveRandom = new Random(0)
  val cells = mutable.ArraySeq(field.cells : _*)
  var itemCount = field.itemCount
  var x = 1;
  var y = 1;

  /** get value of cell on position */
  private def cell(x: Int, y: Int) : Cell.Value = cells(y * field.fieldSize + x)

  def situation : Situation = {
    val sides = IndexedSeq(cell(x, y - 1), cell(x + 1, y), cell(x, y + 1), cell(x - 1, y))
    val canPickup = cell(x, y) == Cell.STUFF;
    Situation(sides, canPickup)
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
      case MoveRandom => act(Decisions.all(moveRandom.nextInt(4)))
      case Stay => 0
      case PickUp => pickUp()
    }
  }

}

object Evaluator {

  val fieldSize = 12;
  val itemCount = 50;

  lazy val testField = createRandomField(0)

  lazy val possibleLocations = for {
    x <- 1 until fieldSize - 1
    y <- 1 until fieldSize - 1
  } yield (x + y * fieldSize)

  lazy val wallLocations = (for {
    x <- 0 until fieldSize
    y <- 0 until fieldSize if (y == 0 || x == 0 || x == fieldSize - 1 || y == fieldSize - 1)
  } yield (x + y * fieldSize)).toSet

  /** create deterministic random field from given random seed */
  def createRandomField(seed: Int) = {
    val random = new Random(seed)
    val locations = random.shuffle(possibleLocations).take(itemCount).toSet
    val cells = (0 until fieldSize * fieldSize).map(index =>
      if (wallLocations.contains(index)) Cell.WALL
      else if (locations.contains(index)) Cell.STUFF
      else Cell.EMPTY
    )
    Field(fieldSize, itemCount, cells)
  }

  /** get points for given candidate */
  def evaluate(candidate: Candidate) : Int = {
    val game = new Game(testField)
    var points = 0
    for (i <- 0 until 200) {
      val index = game.situationIndex
      val decision = candidate.decisions(index)
      val p = game.act(decision)
      //println(decision + ": " + p)
      points += p
    }
    points
  }

}
