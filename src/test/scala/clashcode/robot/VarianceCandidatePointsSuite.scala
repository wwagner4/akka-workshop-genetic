package clashcode.robot

import org.scalatest.FeatureSpec
import org.scalatest.matchers.ShouldMatchers

class VarianceCandidatePointsSuite extends FeatureSpec with ShouldMatchers {

  val filler100 = "0123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789"
  val filler24 = "012345678901234567890123"
  val filler = filler100 + filler24
    
  feature("Calculate variance of a set of CandidatePoints") {
    scenario("Variance of two equal CandidatePoints should be 0.0") {
      {
        println(s"size=${("1234" + filler).size}")
        val c1: CandidatePoints = createCandidatePoint("1234" + filler)
        val c2: CandidatePoints = createCandidatePoint("1234" + filler)
        new VarianceCandidatePoints().variance(Seq(c1, c2))
      } should equal(0.0)
    }
    scenario("Variance of two CandidatePoints that are different on one position should be about 1.0") {
      {
        val c1: CandidatePoints = createCandidatePoint("1234" + filler)
        val c2: CandidatePoints = createCandidatePoint("0234" + filler)
        new VarianceCandidatePoints().variance(Seq(c1, c2))
      } should equal(1.0 plusOrMinus(0.1))
    }
    scenario("Variance of two CandidatePoints that are different on two positions should be about 2.0") {
      {
        val c1: CandidatePoints = createCandidatePoint("1234" + filler)
        val c2: CandidatePoints = createCandidatePoint("0034" + filler)
        new VarianceCandidatePoints().variance(Seq(c1, c2))
      } should equal(2.0 plusOrMinus(0.1))
    }
  }

  def createCandidatePoint(integers: String): CandidatePoints = {
    val bits = createArrayOfBytes(integers)
    val code = new CandidateCode(bits)
    new CandidatePoints(code, 0)
  }
  def createArrayOfBytes(in: String): Array[Byte] = {
    in.map(c => c.toByte).toArray
  }
}