package clashcode

import clashcode.robot._

/**
 * Creates 'poolSize' random Candidates.
 */
case class RandomCandidates(poolSize: Int) extends InitialCandidatesFactory {

  def createCodes: Seq[CandidateCode] = {
    (1 to poolSize).map(_ => Situations.getRandomCode)
  }

}

object RandomCandidates {

  /**
   * Creates 200 random candidates
   */
  def defaultSize: InitialCandidatesFactory = RandomCandidates(200)

}

/**
 * Creates an initial population of 'poolSize' candidates. Some of them
 * are fixed ('fixedCandidates') and the rest are random candidates
 * The fixed candidates are defined by a string, where every character defines one of the
 * six possible actions
 */
case class SomeFixedCandidates(poolSize: Int, fixedCandidates: Seq[String]) extends InitialCandidatesFactory {

  def createCodes: Seq[CandidateCode] = {
    if (fixedCandidates.size >= poolSize) {
      fixedCandidates.take(poolSize).map(codeFromString(_))
    } else {
      val fixed = fixedCandidates.map(codeFromString(_))
      val random = (1 to (poolSize - fixed.size)).map(_ => Situations.getRandomCode)
      fixed ++ random
    }
  }

  private def codeFromString(in: String): CandidateCode = {
    CandidateCode(in.map(_.toString.toByte).toArray)
  }

}

object SomeFixedCandidates {

  /**
   * Contains four fixed candidates that where breeded in previos sessions
   */
  def fourFixed01: InitialCandidatesFactory = {
    val codes = Seq(
      "51301322330032512311322312522423201152150530450550520250130242234444434444444454444434444434434445443443400051000030503450000004",
      "02111252040352002511322422442522205234212001404314432514511510105404452400044140444432403434434531442544242403235544523311422344",
      "32311322203022022011312022322233444231200100502320305542121211434444404444443551414444454444444400144340434442442444442424414144",
      "22140342305312242310422532532534551101121534514202154423035242453423451401113222412421454444440514444444440023535310500505215104")

    SomeFixedCandidates(200, codes)
  }
}

