package clashcode

import java.io.FileOutputStream
import java.text.SimpleDateFormat
import java.util.Date
import scala.collection.GenSeq

import clashcode.robot._

object Main extends App {

  // Create evolution
  val ev = {

    val iniFac: InitialCandidatesFactory = RandomCandidates.defaultSize
    //val iniFac: InitialCandidatesFactory = initial.SomeFixedCandidates.fourFixed01

    //val genOpStrat: GeneticOperationsStrategy = ChrisGenOpStrategy
    //val genOpStrat: GeneticOperationsStrategy = SillyGenOpStrategy_01
    //val genOpStrat: GeneticOperationsStrategy = SillyGenOpStrategy_02
    //val genOpStrat: GeneticOperationsStrategy = SillyGenOpStrategy_03
    val genOpStrat: GeneticOperationsStrategy = SillyGenOpStrategy_04(0.1)

    val popBuildStrat: PopulationBuildingStrategy = ChrisPopBuildStrategy

    val debugStrategy: DebugStrategy = StdoutDebugStrategy

    new Evolution(iniFac, genOpStrat, popBuildStrat, debugStrategy)
  }

  val start = System.currentTimeMillis
  val ts: String = createTimestamp
  (0 until 300).foreach {
    i =>
      {
        ev.tick(20)
        ev.debug()
        save(s"best-$ts.txt", ev.candidates.head.code.bits)
      }
  }
  val done = System.currentTimeMillis - start
  println(s"done $done ms")

  // Some Utility functions

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

