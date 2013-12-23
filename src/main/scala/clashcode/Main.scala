package clashcode

import java.io.FileOutputStream
import clashcode.robot.CandidateCode
import clashcode.robot.Evolution
import clashcode.robot.Situations
import java.text.SimpleDateFormat
import java.util.Date
import clashcode.robot.InitialCandidatesFactory
import clashcode.robot.SelectionStrategy
import clashcode.robot.CandidatePoints
import clashcode.robot.Couple
import clashcode.robot.CrossoverStrategy
import clashcode.robot.Decisions

object Main extends App {

  val ts: String = createTimestamp

  //val fac: InitialCandidatesFactory = initial.RandomCandidates.defaultSize
  val fac: InitialCandidatesFactory = initial.SomeFixedCandidates.fourFixed01

  val selStrat = selection.RandomSelectionStrategy.pairwiseRandom
  
  val crossStrat = crossover.ChrisCrossoverStrategy()

  val ev = new Evolution(fac, selStrat, crossStrat)
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

/**
 * Implementations for the InitialCandidatesFactory
 */
package initial {

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

}

package selection {

  object RandomSelectionStrategy {

    def pairwiseRandom: SelectionStrategy = RandomSelectionStrategy(new java.util.Random())

  }

  case class RandomSelectionStrategy(random: java.util.Random) extends SelectionStrategy {

    def selectCouples(orderedCandidates: Seq[CandidatePoints]): Seq[Couple] = {
      val cnt = orderedCandidates.size
      def randomCouple: Couple = {
        val i1 = random.nextInt(cnt)
        val i2 = random.nextInt(cnt)
        Couple(orderedCandidates(i1), orderedCandidates(i2))
      }
      (1 to cnt).map(_ => randomCouple)
    }

  }

}

package crossover {

  case class ChrisCrossoverStrategy extends CrossoverStrategy {

    val random = new java.util.Random

    def createChildren(generation: Int, couples: Seq[Couple], candidates: Seq[CandidatePoints]): Seq[CandidateCode] = {
      val candidateHashes: Seq[Int] = candidates.map(_.code.bits.toList.hashCode)
      couples.map(couple => crossover(generation, couple, candidateHashes))
    }

    private def crossover(generation: Int, couple: Couple, candidateHashes: Seq[Int]): CandidateCode = {

      val left = couple.left.code
      val right = couple.right.code

      // crossover
      val leftCount = random.nextInt(left.bits.length)
      val result = left.bits.take(leftCount) ++ right.bits.drop(leftCount)

      // mutate
      val mutResult = Math.pow(2 + (generation * Situations.codeLength) / 10000.0, -1) * 100
      //println(mutResult)
      val mutateCount = mutResult.toInt.max(1)
      do {
        for (i <- 0 until mutateCount) {
          result(random.nextInt(result.length)) = random.nextInt(Decisions.count).toByte
        }
      } while ({
        // keep mutating if duplicate candidate
        val compareList = result.toList.hashCode
        val mutate = candidateHashes.contains(compareList)
        //if (mutate) println("keep mutating")
        mutate
      })

      CandidateCode(result)
    }

  }

}
