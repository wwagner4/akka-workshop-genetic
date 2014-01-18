package clashcode.robot

import scala.util.Random
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.ParSeq
import scala.collection.GenSeq

/**
 *
 */
class Evolution(
    initials: InitialCandidatesFactory, 
    genOpStrat: GeneticOperationsStrategy, 
    popBuildStrat: PopulationBuildingStrategy,
    debugStrategy: DebugStrategy) {

  var candidates = initials.createCodes.map(c => c.evaluate).toSeq
  val poolSize = candidates.size

  var random = new Random()
  var generation = 0

  val taskSupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool((Seq.empty[Int].par.tasksupport.parallelismLevel * 15) / 10))
  //println(taskSupport.parallelismLevel)

  tick()

  def tick(count: Int): CandidatePoints = {
    (1 until count).foreach(_ => tick())
    tick()
  }

  def tick(): CandidatePoints = {

    generation += 1

    // create next generation candidates
    val newCodes = genOpStrat.createNewMembers(generation, candidates)
    //println(s"newCodes size: ${newCodes.size}")

    // evaluate next generation
    //val newPoints = newCodes.par.map(_.evaluate)
    val par = newCodes.par
    par.tasksupport = taskSupport
    val newPoints = par.map(_.evaluate)
    candidates = popBuildStrat.createNextPopulation(generation, poolSize, newPoints, candidates)
    candidates(0) // return best candidate
  }

  def debug(iteration: Int): Unit = debugStrategy.debug(iteration, generation, candidates)
}

trait InitialCandidatesFactory {

  /**
   * Creates an initial population of candidates
   */
  def createCodes: Seq[CandidateCode]

}

/**
 * Defines how members of the next generation are created
 */
trait GeneticOperationsStrategy {

  /**
   *  Create new members of the next generation.
   *  To do so perform the following steps
   *  - Select couples of candidates to be the parents for some of the members
   *    of the next generation
   *  - create new candidates by applying crossover on the selected couples
   *  - Apply mutation on the outcome of crossing over the couples (optional)
   *  - Apply mutation on any of the candidates from the previous generation (optional)
   *  - Create new random candidates (optional)
   *
   *  generation:         The number of the processed generation
   *  previousGeneration: The candidates from the previous generation sorted by their
   *                      fitness
   */

  def createNewMembers(generation: Int, previousGeneration: Seq[CandidatePoints]): Seq[CandidateCode]

}

/**
 * Defines how populations are created after new members where created and tested
 */
trait PopulationBuildingStrategy {

  /**
   * Creates the next population after new members where created and their fitness was tested
   * Here you can descide how many members are taken from the new generated members, 
   * how many are taken from the previous generation and if some new random members are 
   * added.
   */
  def createNextPopulation(generation: Int, poolSize: Int, newMembers: GenSeq[CandidatePoints], previousGeneration: Seq[CandidatePoints]): Seq[CandidatePoints]
}

/**
 * Debug strategy.
 * Defines how debugging takes place
 */

trait DebugStrategy {
  
  def debug(iteration: Int, generation: Int,candidates: Seq[CandidatePoints])
  
}