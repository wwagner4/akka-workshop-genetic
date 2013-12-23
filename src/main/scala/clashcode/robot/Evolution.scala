package clashcode.robot

import scala.util.Random
import scala.collection.parallel.ForkJoinTaskSupport


/**
 * 
 */
class Evolution(initials: InitialCandidatesFactory, selStrat: SelectionStrategy, crossStrat: CrossoverStrategy) {

  var candidates = initials.createCodes.map(c => c.evaluate).toSeq
  val poolSize = candidates.size
  
  var random = new Random()

  var generation = 0
  var firstDebug = true
  
  val variCalc = new VarianceCandidatePoints()

  val taskSupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool((Seq.empty[Int].par.tasksupport.parallelismLevel * 15) / 10))
  //println(taskSupport.parallelismLevel)

  tick()


  def tick(count: Int) : CandidatePoints = {
    (1 until count).foreach(_ => tick())
    tick()
  }

  def tick() : CandidatePoints = {

    generation += 1

    // create next generation candidates
    val couples = selStrat.selectCouples(candidates)
    val newCodes = crossStrat.createNewMembers(generation, couples, candidates)
    //println(s"newCodes size: ${newCodes.size}")
    
    // evaluate next generation
    //val newPoints = newCodes.par.map(_.evaluate)
    val par = newCodes.par
    par.tasksupport = taskSupport
    val newPoints = par.map(_.evaluate)

    // get pool of best
    val allCandidates = candidates ++ newPoints
    val bestCandidates = allCandidates.sortBy(- _.points).take(poolSize)
    candidates = bestCandidates

    bestCandidates(0) // return best candidate
  }

  def debug() {
    //mutateCount < Situations.codeLength / 10
    //if (variability < 0.05) mutateCount += 1
	  
    if (firstDebug) {
      val gen = "gen"
      val first = "first"
      val last = "last"
      val vari = "vari"
      val vari1 = "vari1"
      println(f"$gen%5s\t$first%5s\t$last%5s\t$vari%5s\t$vari1%5s")
      firstDebug = false
    }
    val first = candidates(0).points
    val last = candidates.last.points
    val vari = candidates.map(_.points).distinct.length / candidates.length.toDouble
    val vari1 = variCalc.variance(candidates)
    println(f"$generation%5d\t$first%5d\t$last%5d\t$vari%5.3f\t$vari1%5.3f")
  }

}

trait InitialCandidatesFactory {

  /**
   * Creates an initial population of candidates
   */
  def createCodes: Seq[CandidateCode]

}


case class Couple(left: CandidatePoints, right: CandidatePoints) 

trait SelectionStrategy {
  
  /**
   * Selects couples from a sorted sequence of candidates.
   * The candidates are sorted by their fitness. Index 0 is the fittest candidate  
   */
  def selectCouples(orderedCandidates: Seq[CandidatePoints]): Seq[Couple]
  
  
}


trait CrossoverStrategy {
  
  /**
   *  Create new members of the next generation.
   *  To do so use one of the following techniques
   *  - Apply crossover on any of the provided couples
   *  - Apply mutation on the outcome of crossover couples
   *  - Apply mutation on any of the candidates from the previous generation
   *  - Create new random candidates
   *  - Use combinations of the points above
   *  - Invent something totally new
   *  
   *  generation:         The number of the processed generation
   *  couples:            A set of couples created by the selection strategy out of members of 
   *                      the previous generation
   *  previousGeneration: The candidates from the previous generation sorted by their 
   *                      fitness            
   */ 
  
  def createNewMembers(generation: Int, couples: Seq[Couple], previousGeneration: Seq[CandidatePoints]): Seq[CandidateCode] 
  
}
