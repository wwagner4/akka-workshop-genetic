package clashcode

import clashcode.robot._
import java.io.FileOutputStream
import java.text.SimpleDateFormat
import java.util.Date

object MultiRunner extends App {

  val cntInner = 100 // Number of generations between one call of 'debug()'
  val cntOuter = 40 // Number of repetitions of inner calls. Total number of generations is cntInner * cntOuter
  val cntIter = 5 // Number of iterations for each configuration
  
  case class Conf(id: String, genOpStrat: GeneticOperationsStrategy)

  // Testing how different Strategies perform
  val confs_00 = List(
    Conf("Cris", ChrisGenOpStrategy),
    Conf("S01", SillyGenOpStrategy_01),
    Conf("S02", SillyGenOpStrategy_02()),
    Conf("S03", SillyGenOpStrategy_03()),
    Conf("S04_01", SillyGenOpStrategy_04(0.1)),
    Conf("S04_001", SillyGenOpStrategy_04(0.01)),
    Conf("S04_0001", SillyGenOpStrategy_04(0.001)),
    Conf("S04_00001", SillyGenOpStrategy_04(0.0001)))

  // Testing how different mutation rates perform. Wide range of rates
  val confs_01 = List(
    Conf("S03_10", SillyGenOpStrategy_03(10)),
    Conf("S03_100", SillyGenOpStrategy_03(100)),
    Conf("S03_500", SillyGenOpStrategy_03(500)),
    Conf("S03_1000", SillyGenOpStrategy_03(1000)))

  // Testing how different complex mutation rates perform. Wide range of rates
  // complex means that the mutation rate changes by generation count. 
  // It gets smaller when generation count is higher.
  val confs_02 = List(
    Conf("S03_80", SillyGenOpStrategy_03(80)),
    Conf("S03_90", SillyGenOpStrategy_03(90)),
    Conf("S03_100", SillyGenOpStrategy_03(100)),
    Conf("S03_110", SillyGenOpStrategy_03(110)),
    Conf("S03_120", SillyGenOpStrategy_03(120)))
    
  // Like 02 but with a wider range. The smaller range from 02 brought no
  // significant differences
  val confs_02a = List(
    Conf("S03_80", SillyGenOpStrategy_03(10)),
    Conf("S03_90", SillyGenOpStrategy_03(50)),
    Conf("S03_100", SillyGenOpStrategy_03(100)),
    Conf("S03_110", SillyGenOpStrategy_03(150)),
    Conf("S03_120", SillyGenOpStrategy_03(200)))

  // Testing the performance of different simple mutation rates.
  // Simple mutation rate means, it does not change by generation count
  val confs_03 = List(
    Conf("S02_005", SillyGenOpStrategy_02(0.005)),
    Conf("S02_01", SillyGenOpStrategy_02(0.01)),
    Conf("S02_02", SillyGenOpStrategy_02(0.02)),
    Conf("S02_05", SillyGenOpStrategy_02(0.05)))

  // Choose a configuration list  
  val out = confs_02a.map(c => run(c))

  val buffer = new StringBuilder
  buffer.append(BufferedDebugStrategy.header)
  buffer.append("\n")
  buffer.append(out.mkString(""))
  buffer.append("\n")

  val filename = s"multi-${createTimestamp}.csv"

  save(filename, buffer.toString)

  println(s"FINISHED Multirunner. Saved result to '$filename'")

  /**
   * Run one configuration 'cntIter' times 
   */
  def run(conf: Conf): String = {
    val results = (1 to cntIter).map(iter => runIteration(conf, iter))
    results.mkString("")
  }

  /**
   * Run one configuration
   */
  def runIteration(conf: Conf, iter: Int): String = {
    println(s"start ${conf.id}")
    val debugStrategy = BufferedDebugStrategy(conf.id)
    val ev = {
      val iniFac: InitialCandidatesFactory = RandomCandidates.defaultSize
      val genOpStrat: GeneticOperationsStrategy = conf.genOpStrat
      val popBuildStrat: PopulationBuildingStrategy = ChrisPopBuildStrategy
      new Evolution(iniFac, genOpStrat, popBuildStrat, debugStrategy)
    }

    (1 to cntOuter).foreach(i => {
      ev.tick(cntInner)
      ev.debug(iter)
      println(f"- ${conf.id}%20s $iter%5d / $cntIter $i%5d / $cntOuter")
    })
    println(s"finished ${conf.id}")
    debugStrategy.buffer
  }

  // Some Utility functions

  private def save(name: String, value: String) {
    def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
      val pw = new java.io.PrintWriter(f)
      try { op(pw) } finally { pw.close() }
    }
    printToFile(new java.io.File(name))(pw => pw.println(value))
  }

  private def createTimestamp: String = {
    val sdf = new SimpleDateFormat("yyMMdd-HHmmss")
    sdf.format(new Date())
  }

}