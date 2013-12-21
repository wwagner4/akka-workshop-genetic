package clashcode.robot

import scala.util.Random
import scala.collection.parallel.ForkJoinTaskSupport


/**
 */
class Evolution(poolSize: Int, code: Option[String]) {

  var random = new Random()
  var candidates = code.map(c => CandidateCode(c.map(_.toString.toByte).toArray).evaluate).toSeq
  var candidateHashes = candidates.map(_.code.bits.toList.hashCode)

  var generation = 0

  val taskSupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool((Seq.empty[Int].par.tasksupport.parallelismLevel * 15) / 10))
  println(taskSupport.parallelismLevel)

  tick()

  private def crossover : CandidateCode = {

    val left = candidates(random.nextInt(candidates.length)).code
    val right = candidates(random.nextInt(candidates.length)).code

    // crossover
    val leftCount = random.nextInt(left.bits.length)
    val result = left.bits.take(leftCount) ++ right.bits.drop(leftCount)

    // mutate
    do
    {
      for (i <- 0 until 4) {
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

  def tick(count: Int) : CandidatePoints = {
    (1 until count).foreach(_ => tick())
    tick()
  }

  def tick() : CandidatePoints = {

    generation += 1

    // create next generation candidates
    val newCodes =
      (0 until poolSize - candidates.length).map(_ => Situations.getRandomCode) ++
      (0 until candidates.length).map(_ => crossover)

    // evaluate next generation
    //val newPoints = newCodes.par.map(_.evaluate)
    val par = newCodes.par
    par.tasksupport = taskSupport
    val newPoints = par.map(_.evaluate)

    // get pool of best
    val allCandidates = candidates ++ newPoints
    val bestCandidates = allCandidates.sortBy(- _.points).take(poolSize)
    candidates = bestCandidates
    candidateHashes = bestCandidates.map(_.code.bits.toList.hashCode)

    bestCandidates(0) // return best candidate
  }

  def debug() {
    val a = candidates(0).points
    val b = candidates(1).points
    val c = candidates(2).points
    val d = candidates.last.points
    val v = candidates.map(_.points).distinct.length / candidates.length.toDouble
    println(s"---- generation $generation")
    println(s"     points best three: $a $b $c last: $d")
    println(f"     variabillity: $v%.4f")
  }

}
