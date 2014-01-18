package clashcode

import clashcode.robot.DebugStrategy
import clashcode.robot.CandidatePoints
import clashcode.robot.CandidateVariance

case object StdoutDebugStrategy extends DebugStrategy {

  var firstDebug = true
  val candVari = new CandidateVariance()

  def debug(generation: Int,candidates: Seq[CandidatePoints]) = {
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
    val vari1 = candVari.diffCount(candidates)
    println(f"$generation%5d\t$first%5d\t$last%5d\t$vari%5.3f\t$vari1%5.3f")
  }
  
}