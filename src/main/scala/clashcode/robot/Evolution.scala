package clashcode.robot

import scala.util.Random
import scala.collection.parallel.ForkJoinTaskSupport


/**
 */
class Evolution(poolSize: Int, code: Option[String]) {

  var random = new Random()
  var candidates = code.map(c => CandidatePoints(CandidateCode(c.map(_.toString.toByte).toArray), 10000)).toSeq
  var generation = 0
  tick()

  var variability = 1.0
  var mutateCount = 5

  val taskSupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool((Seq.empty[Int].par.tasksupport.parallelismLevel * 15) / 10))
  println(taskSupport.parallelismLevel)

  private def generate : CandidateCode = {
    if (candidates.length == 0)
      Situations.getRandomCode // generate random candidates
    else {
      val left = candidates(random.nextInt(candidates.length))
      val right = candidates(random.nextInt(candidates.length))
      crossover(left.code, right.code)
    }
  }

  private def crossover(left: CandidateCode, right: CandidateCode) : CandidateCode = {

    // crossover
    val leftCount = random.nextInt(left.bits.length)
    val result = left.bits.take(leftCount) ++ right.bits.drop(leftCount)

    // mutate
    for (i <- 0 until mutateCount) {
      result(random.nextInt(result.length)) = random.nextInt(Decisions.count).toByte
    }

    CandidateCode(result)
  }

  def tick(count: Int) : CandidatePoints = {
    (1 until count).foreach(_ => tick())
    tick()
  }

  def tick() : CandidatePoints = {
    generation += 1

    // create next generation candidates
    val newCodes = (0 until poolSize).map(_ => generate)

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
    println("Best: " + candidates.take(3).map(_.points).mkString(", "))

    variability = candidates.map(_.points).distinct.length / candidates.length.toDouble
    //mutateCount < Situations.codeLength / 10
    if (variability < 0.05) mutateCount += 1

    println("Worst: " + candidates.last.points + ", mut: " + mutateCount + ", var: " + variability)
  }

}
