package clashcode

import clashcode.robot.CandidatePoints

case class RandomSelectionStrategy(random: java.util.Random) extends SelectionStrategy {

  def select(previousGeneration: Seq[CandidatePoints]): LeftRight = {
    val left = previousGeneration(random.nextInt(previousGeneration.size)).code
    val right = previousGeneration(random.nextInt(previousGeneration.size)).code
    LeftRight(left, right)
  }

}

/**
 * Select the fittest and a random candidate
 */
case class AlphaSelectionStrategy(random: java.util.Random) extends SelectionStrategy {

  def select(previousGeneration: Seq[CandidatePoints]): LeftRight = {
    if (random.nextBoolean())
      LeftRight(previousGeneration(0).code,
        previousGeneration(random.nextInt(previousGeneration.size)).code)
    else
      LeftRight(previousGeneration(0).code,
        previousGeneration(random.nextInt(previousGeneration.size)).code)
  }
}

/**
 * Select one of the group of fittest and another random candidate
 */
case class AlphaGroupSelectionStrategy(random: java.util.Random, groupSize: Int = 10) extends SelectionStrategy {

  def select(previousGeneration: Seq[CandidatePoints]): LeftRight = {
    require(previousGeneration.size >= groupSize)
    if (random.nextBoolean())
      LeftRight(previousGeneration(random.nextInt(groupSize)).code,
        previousGeneration(random.nextInt(previousGeneration.size)).code)
    else
      LeftRight(previousGeneration(random.nextInt(groupSize)).code,
        previousGeneration(random.nextInt(previousGeneration.size)).code)
  }
}

/**
 * Select one of the group of fittest and another random candidate
 */
case class GroupOfFittestSelectionStrategy(random: java.util.Random, groupSize: Int = 10) extends SelectionStrategy {

  def select(previousGeneration: Seq[CandidatePoints]): LeftRight = {
    require(previousGeneration.size >= groupSize)
    if (random.nextBoolean())
      LeftRight(previousGeneration(random.nextInt(groupSize)).code,
        previousGeneration(random.nextInt(groupSize)).code)
    else
      LeftRight(previousGeneration(random.nextInt(groupSize)).code,
        previousGeneration(random.nextInt(groupSize)).code)
  }
}

