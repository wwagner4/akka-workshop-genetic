package clashcode

import java.io.FileOutputStream
import clashcode.robot.CandidateCode
import clashcode.robot.Evolution
import clashcode.robot.Situations
import java.text.SimpleDateFormat
import java.util.Date
import clashcode.robot.CandidateCodeFactory

object Main extends App {

  val ts: String = createTimestamp

  val codes = Seq(
    "51301322330032512311322312522423201152150530450550520250130242234444434444444454444434444434434445443443400051000030503450000004",
    "02111252040352002511322422442522205234212001404314432514511510105404452400044140444432403434434531442544242403235544523311422344",
    "32311322203022022011312022322233444231200100502320305542121211434444404444443551414444454444444400144340434442442444442424414144",
    "22140342305312242310422532532534551101121534514202154423035242453423451401113222412421454444440514444444440023535310500505215104")

  val fac:CandidateCodeFactory = factory.SomeFixedCandidates(200, codes)
  //val fac:CandidateCodeFactory = factory.RandomCandidates(200)
  
  val ev = new Evolution(fac)
  val start = System.currentTimeMillis
  (0 until 300).foreach {
    i =>
      {
        ev.tick(20)
        ev.debug()
        save(s"best-$ts.txt", ev.candidates.head.code.bits)
      }
  }
  val done = System.currentTimeMillis - start
  println(done)

  def save(name: String, array: Seq[Byte]) {
    val o = new FileOutputStream(name)
    o.write(array.mkString.getBytes)
    o.close()
  }

  def createTimestamp: String = {
    val sdf = new SimpleDateFormat("yyMMdd-HHmmss")
    sdf.format(new Date())
  }

}

package factory {

  case class RandomCandidates(poolSize: Int) extends CandidateCodeFactory {

    def createCodes: Seq[CandidateCode] = {
      (1 to poolSize).map(_ => Situations.getRandomCode)
    }

  }

  case class SomeFixedCandidates(poolSize: Int, fixedCandidates: Seq[String]) extends CandidateCodeFactory {

    def createCodes: Seq[CandidateCode] = {
      if (fixedCandidates.size >= poolSize) {
        fixedCandidates.take(poolSize).map(codeFromString(_))
      } else {
        val fixed = fixedCandidates.map(codeFromString(_))
        val random = (1 to (poolSize - fixed.size)).map(_ => Situations.getRandomCode)
        fixed ++ random
      }
    }

    def codeFromString(in: String): CandidateCode = {
      CandidateCode(in.map(_.toString.toByte).toArray)
    }

  }

}


