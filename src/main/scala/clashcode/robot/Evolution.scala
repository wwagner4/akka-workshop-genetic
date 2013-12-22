package clashcode.robot

import scala.util.Random
import scala.collection.parallel.ForkJoinTaskSupport


/**
 * 
 * 
 */
class Evolution(poolSize: Int, code: String) {

  var random = new Random()
  var candidates: Seq[CandidatePoints] = Seq(CandidateCode(code.map(_.toString.toByte).toArray).evaluate)
  var candidateHashes: Seq[Int] = candidates.map(_.code.bits.toList.hashCode)

  var generation = 0

  val taskSupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool((Seq.empty[Int].par.tasksupport.parallelismLevel * 15) / 10))
  println(taskSupport.parallelismLevel)

  tick()

  /**
   * Performs all the necessary steps during one generation
   */
  def tick() : CandidatePoints = {

    generation += 1

    // create next generation candidates
    val newCodes =
      (0 until poolSize - candidates.length).map(_ => Situations.getRandomCode) ++
      (0 until candidates.length).map(_ => crossover)

    // evaluate next generation
    //val newPoints = newCodes.par.map(_.evaluate)
    val newCodesPar = newCodes.par
    newCodesPar.tasksupport = taskSupport
    val newPoints = newCodesPar.map(_.evaluate)

    // get pool of best
    val allCandidates = candidates ++ newPoints
    val bestCandidates = allCandidates.sortBy(- _.points).take(poolSize)
    candidates = bestCandidates
    candidateHashes = bestCandidates.map(_.code.bits.toList.hashCode)

    bestCandidates(0) // return best candidate
  }

  def tick(count: Int) : CandidatePoints = {
    (1 until count).foreach(_ => tick())
    tick()
  }

  private def crossover : CandidateCode = {

    val left = candidates(random.nextInt(candidates.length)).code
    val right = candidates(random.nextInt(candidates.length)).code

    // crossover
    val leftCount = random.nextInt(left.bits.length)
    val result = left.bits.take(leftCount) ++ right.bits.drop(leftCount)

    // mutate
    //mutateCount < Situations.codeLength / 10
    //if (variability < 0.05) mutateCount += 1
    //println(mutResult)
    val mutResult = Math.pow(2 + (generation * Situations.codeLength) / 10000.0, -1) * 100
    val mutateCount = mutResult.toInt.max(1)
    do
    {
      for (i <- 0 until mutateCount) {
        result(random.nextInt(result.length)) = random.nextInt(Decisions.count).toByte
      }
    }
    while({
      // keep mutating if duplicate candidate
      val compareList = result.toList.hashCode
      val mutate = candidateHashes.contains(compareList)
      //if (mutate) println("keep mutating")
      mutate
    })

    CandidateCode(result)
  }

  def debug() {
    val a = candidates(0).points
    val b = candidates(1).points
    val c = candidates(2).points
    val last = candidates.last.points
    val vari = candidates.map(_.points).distinct.length / candidates.length.toDouble
    println(f"$generation%d5\t$a%d5\t$b%d5\t$c%d5\t$last%d5\t$vari%5.3f")
  }

}
