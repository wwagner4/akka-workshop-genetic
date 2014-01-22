package clashcode

import clashcode.robot.CandidatePoints

case class RandomSelectionStrategy(random: java.util.Random) extends SelectionStrategy {

  def select(previousGeneration: Seq[CandidatePoints]): LeftRight = {
    val left = previousGeneration(random.nextInt(previousGeneration.size)).code
    val right = previousGeneration(random.nextInt(previousGeneration.size)).code
    LeftRight(left, right)
  }

}

case class AlphaSelectionStrategy(random: java.util.Random) extends SelectionStrategy {

  def select(previousGeneration: Seq[CandidatePoints]): LeftRight = {
    // select the fittest and a random candidate
    if (random.nextBoolean())
      LeftRight(previousGeneration(0).code,
        previousGeneration(random.nextInt(previousGeneration.size)).code)
    else
      LeftRight(previousGeneration(0).code,
        previousGeneration(random.nextInt(previousGeneration.size)).code)
  }
}

