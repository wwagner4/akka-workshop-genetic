package clashcode

import java.io.FileOutputStream
import java.text.SimpleDateFormat
import java.util.Date
import scala.collection.GenSeq

import clashcode.robot._

object Main extends App {

  val ts: String = createTimestamp

  val iniFac: InitialCandidatesFactory = RandomCandidates.defaultSize
  //val iniFac: InitialCandidatesFactory = initial.SomeFixedCandidates.fourFixed01

  //val genOpStrat = ChrisGenOpStrategy
  //val genOpStrat = SillyGenOpStrategy_01
  //val genOpStrat = SillyGenOpStrategy_02
  //val genOpStrat = SillyGenOpStrategy_03
  val genOpStrat = SillyGenOpStrategy_04(0.1)

  val popBuildStrat = ChrisPopBuildStrategy

  val ev = new Evolution(iniFac, genOpStrat, popBuildStrat)
  val start = System.currentTimeMillis
  (0 until 300).foreach {
    i =>
      {
        ev.tick(20)
        ev.debug()
        save(s"best-$ts.txt", ev.candidates.head.code.bits)
      }
  }
  //val done = System.currentTimeMillis - start
  //println(done)

  private def save(name: String, array: Seq[Byte]) {
    val o = new FileOutputStream(name)
    o.write(array.mkString.getBytes)
    o.close()
  }

  private def createTimestamp: String = {
    val sdf = new SimpleDateFormat("yyMMdd-HHmmss")
    sdf.format(new Date())
  }

}

// Implementations for GeneticOperationsStrategy

