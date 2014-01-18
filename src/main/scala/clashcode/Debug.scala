package clashcode

import clashcode.robot.DebugStrategy
import clashcode.robot.CandidatePoints
import clashcode.robot.CandidateVariance

case object StdoutDebugStrategy extends DebugStrategy {

  var firstDebug = true
  val candVari = new CandidateVariance()

  val sepa = "\t"

  def debug(iteration: Int, generation: Int, candidates: Seq[CandidatePoints]) = {
    //mutateCount < Situations.codeLength / 10
    //if (variability < 0.05) mutateCount += 1

    if (firstDebug) {
      val gen = "gen"
      val first = "first"
      val last = "last"
      val vari = "vari"
      val vari1 = "vari1"
      println(f"$gen%5s$sepa$first%5s$sepa$last%5s$sepa$vari%5s$sepa$vari1%5s")
      firstDebug = false
    }
    val first = candidates(0).points
    val last = candidates.last.points
    val vari = candidates.map(_.points).distinct.length / candidates.length.toDouble
    val vari1 = candVari.diffCount(candidates)
    println(f"$generation%5d$sepa$first%5d$sepa$last%5d$sepa$vari%5.3f$sepa$vari1%5.3f")
  }

}

case class BufferedDebugStrategy(id: String) extends DebugStrategy {

  private val sb = new StringBuilder()
  private val candVari = new CandidateVariance()

  def debug(iteration: Int, generation: Int, candidates: Seq[CandidatePoints]) = {
    val first = candidates(0).points
    val last = candidates.last.points
    val vari = candidates.map(_.points).distinct.length / candidates.length.toDouble
    val vari1 = candVari.diffCount(candidates)
    sb.append(BufferedDebugStrategy.vForm format (id, iteration, generation, first, last, vari, vari1))
    sb.append("\n")
  }

  def buffer: String = sb.toString

}

object BufferedDebugStrategy {
  def sepa = ";"

  val hFormat = List(
    "%20s",
    "%5s",
    "%5s",
    "%5s",
    "%5s",
    "%5s",
    "%5s")

  val vFormat = List(
    "%20s",
    "%5d",
    "%5d",
    "%5d",
    "%5d",
    "%5.2f",
    "%5.2f")

  val hForm = hFormat.mkString(sepa)
  val vForm = vFormat.mkString(sepa)

  def header: String = hForm format ("id", "iter", "gen", "first", "last", "vari1", "vari2")

}
