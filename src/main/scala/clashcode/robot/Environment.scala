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

  val cells = mutable.ArraySeq(field.cells : _*)
  var itemCount = field.itemCount
  var x = 1;
  var y = 1;

  /** get value of cell on position */
  def cell(x: Int, y: Int) : Cell.Value = cells(y * field.fieldSize + x)

  def situation : Situation = {
    val sides = IndexedSeq(cell(x, y - 1), cell(x + 1, y), cell(x, y + 1), cell(x - 1, y))
    val canPickup = cell(x, y) == Cell.STUFF;
    Situation(sides, canPickup)
  }

  def situationIndex : Int = Situations.getIndex(situation)

  /** pick up item if possible */
  def pickUp() : Boolean = {
    val index = y * field.fieldSize + x
    if (cells(index) == Cell.STUFF)
    {
      itemCount -= 1
      cells(index) = Cell.EMPTY
      true
    }
    else
      false
  }

}

object Evaluator {

  val fieldSize = 10;
  val itemCount = 20;

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

      // pick up object
      val decision = candidate.decisions(index)
      if (decision.pickUp && game.pickUp())
        points += 1

      // move robot
      val nextX = game.x + decision.direction.x
      val nextY = game.y + decision.direction.y
      if (game.cell(nextX, nextY) != Cell.WALL)
      {
        game.x = nextX
        game.y = nextY
      }
      //println(game.x + " / " + game.y)
    }
    points
  }

}
