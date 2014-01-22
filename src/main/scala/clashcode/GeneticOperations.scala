package clashcode

import clashcode.robot._

case object ChrisGenOpStrategy extends GeneticOperationsStrategy {

  val random = new java.util.Random

  def createNewMembers(generation: Int, previousGeneration: Seq[CandidatePoints]): Seq[CandidateCode] = {
    val candidateHashes: Seq[Int] = previousGeneration.map(_.code.bits.toList.hashCode)
    def createNewCandidate: CandidateCode = {

      // select
      val left = previousGeneration(random.nextInt(previousGeneration.size)).code
      val right = previousGeneration(random.nextInt(previousGeneration.size)).code

      // crossover
      val leftCount = random.nextInt(left.bits.length)
      val result = left.bits.take(leftCount) ++ right.bits.drop(leftCount)

      // mutate
      val mutResult = Math.pow(2 + (generation * Situations.codeLength) / 10000.0, -1) * 100
      //println(mutResult)
      val mutateCount = mutResult.toInt.max(1)
      do {
        for (i <- 0 until mutateCount) {
          result(random.nextInt(result.length)) = random.nextInt(Decisions.count).toByte
        }
      } while ({
        // keep mutating if duplicate candidate
        val compareList = result.toList.hashCode
        val mutate = candidateHashes.contains(compareList)
        //if (mutate) println("keep mutating")
        mutate
      })
      CandidateCode(result)
    }
    (1 to previousGeneration.size) map (_ => createNewCandidate)
  }

}

/**
 * Based on CrisGenOpStrategy. 
 * - Alpha selection strategy. Selects always the fittest and a random candidate.
 */
case object SillyGenOpStrategy_01 extends GeneticOperationsStrategy {

  case class LeftRight(left: CandidateCode, right: CandidateCode)

  val random = new java.util.Random

  def createNewMembers(generation: Int, previousGeneration: Seq[CandidatePoints]): Seq[CandidateCode] = {
    val candidateHashes: Seq[Int] = previousGeneration.map(_.code.bits.toList.hashCode)
    def createNewCandidate: CandidateCode = {

      // select the fittest and a random candidate
      val lr = if (random.nextBoolean())
        LeftRight(previousGeneration(0).code,
          previousGeneration(random.nextInt(previousGeneration.size)).code)
      else
        LeftRight(previousGeneration(0).code,
          previousGeneration(random.nextInt(previousGeneration.size)).code)

      // crossover
      val leftCount = random.nextInt(lr.left.bits.length)
      val result = (lr.left).bits.take(leftCount) ++ (lr.right).bits.drop(leftCount)

      // mutate
      val mutResult = Math.pow(2 + (generation * Situations.codeLength) / 10000.0, -1) * 100
      //println(mutResult)
      val mutateCount = mutResult.toInt.max(1)
      do {
        for (i <- 0 until mutateCount) {
          result(random.nextInt(result.length)) = random.nextInt(Decisions.count).toByte
        }
      } while ({
        // keep mutating if duplicate candidate
        val compareList = result.toList.hashCode
        val mutate = candidateHashes.contains(compareList)
        //if (mutate) println("keep mutating")
        mutate
      })
      CandidateCode(result)
    }
    (1 to previousGeneration.size) map (_ => createNewCandidate)
  }

}

/**
 * Based on CrisGenOpStrategy. 
 * - Simplified calculation of 'mutateCount'
 */
case class SillyGenOpStrategy_02(mutationRate: Double = 0.01) extends GeneticOperationsStrategy {

  val random = new java.util.Random

  def createNewMembers(generation: Int, previousGeneration: Seq[CandidatePoints]): Seq[CandidateCode] = {
    val candidateHashes: Seq[Int] = previousGeneration.map(_.code.bits.toList.hashCode)
    def createNewCandidate: CandidateCode = {

      // select the fittest and a random candidate
      val left = previousGeneration(random.nextInt(previousGeneration.size)).code
      val right = previousGeneration(random.nextInt(previousGeneration.size)).code

      // crossover
      val leftCount = random.nextInt(left.bits.length)
      val result = left.bits.take(leftCount) ++ right.bits.drop(leftCount)

      // Must be at least 1. Otherwise you get int an endless loop
      val mutateCount = math.max((Situations.codeLength * mutationRate).toInt, 1)
      do {
        for (i <- 1 to mutateCount) {
          result(random.nextInt(result.length)) = random.nextInt(Decisions.count).toByte
        }
      } while ({
        // keep mutating if duplicate candidate
        val compareList = result.toList.hashCode
        val mutate = candidateHashes.contains(compareList)
        //if (mutate) println("keep mutating")
        mutate
      })
      CandidateCode(result)
    }
    (1 to previousGeneration.size) map (_ => createNewCandidate)
  }
}

/**
 * Based on CrisGenOpStrategy. 
 * - Do not check for duplicates
 */
case class SillyGenOpStrategy_03(mutation: Int = 100) extends GeneticOperationsStrategy {

  val random = new java.util.Random

  def createNewMembers(generation: Int, previousGeneration: Seq[CandidatePoints]): Seq[CandidateCode] = {
    def createNewCandidate: CandidateCode = {

      // select
      val left = previousGeneration(random.nextInt(previousGeneration.size)).code
      val right = previousGeneration(random.nextInt(previousGeneration.size)).code

      // crossover
      val leftCount = random.nextInt(left.bits.length)
      val result = left.bits.take(leftCount) ++ right.bits.drop(leftCount)

      // mutate
      val mutResult = Math.pow(2 + (generation * Situations.codeLength) / 10000.0, -1) * mutation
      //println(mutResult)
      val mutateCount = mutResult.toInt.max(1)
      for (i <- 0 until mutateCount) {
        result(random.nextInt(result.length)) = random.nextInt(Decisions.count).toByte
      }
      CandidateCode(result)
    }
    (1 to previousGeneration.size) map (_ => createNewCandidate)
  }
}

/**
 * Based on CrisGenOpStrategy. 
 * - Do not check for duplicates
 * - Simplified mutation rate (configurable)
 */
case class SillyGenOpStrategy_04(mutationRate: Double) extends GeneticOperationsStrategy {

  val random = new java.util.Random

  def createNewMembers(generation: Int, previousGeneration: Seq[CandidatePoints]): Seq[CandidateCode] = {
    def createNewCandidate: CandidateCode = {

      // select
      val left = previousGeneration(random.nextInt(previousGeneration.size)).code
      val right = previousGeneration(random.nextInt(previousGeneration.size)).code

      // crossover
      val leftCount = random.nextInt(left.bits.length)
      val result = left.bits.take(leftCount) ++ right.bits.drop(leftCount)

      // mutate
      val mutateCount = math.max((Situations.codeLength * mutationRate).toInt, 1)
      for (i <- 0 until mutateCount) {
        result(random.nextInt(result.length)) = random.nextInt(Decisions.count).toByte
      }
      CandidateCode(result)
    }
    (1 to previousGeneration.size) map (_ => createNewCandidate)
  }
}

