package clashcode

import clashcode.robot._
import java.io.FileOutputStream
import java.text.SimpleDateFormat
import java.util.Date

object MultiRunner extends App {

  val cntInner = 100 // Number of generations between one call of 'debug()'
  val cntOuter = 40 // Number of repetitions of inner calls. Total number of generations is cntInner * cntOuter
  val cntIter = 5 // Number of iterations for each configuration

  val random = new java.util.Random

  val selStrat = RandomSelectionStrategy(random)
  //val selStrat = AlphaSelectionStrategy(random)

  case class Conf(id: String, genOpStrat: GeneticOperationsStrategy)

  // Testing how different Strategies perform
  val confs_00 = List(
    Conf("Cris", ChrisGenOpStrategy(selStrat)),
    Conf("S01", SillyGenOpStrategy_01(selStrat)),
    Conf("S02", SillyGenOpStrategy_02(selStrat = selStrat)),
    Conf("S03", SillyGenOpStrategy_03(selStrat = selStrat)),
    Conf("S04_01", SillyGenOpStrategy_04(0.1, selStrat)),
    Conf("S04_001", SillyGenOpStrategy_04(0.01, selStrat)),
    Conf("S04_0001", SillyGenOpStrategy_04(0.001, selStrat)),
    Conf("S04_00001", SillyGenOpStrategy_04(0.0001, selStrat)))

  // Testing how different mutation rates perform. Wide range of rates
  val confs_01 = List(
    Conf("S03_10", SillyGenOpStrategy_03(10, selStrat)),
    Conf("S03_100", SillyGenOpStrategy_03(100, selStrat)),
    Conf("S03_500", SillyGenOpStrategy_03(500, selStrat)),
    Conf("S03_1000", SillyGenOpStrategy_03(1000, selStrat)))

  // Testing how different complex mutation rates perform. Wide range of rates
  // complex means that the mutation rate changes by generation count. 
  // It gets smaller when generation count is higher.
  val confs_02 = List(
    Conf("S03_80", SillyGenOpStrategy_03(80, selStrat)),
    Conf("S03_90", SillyGenOpStrategy_03(90, selStrat)),
    Conf("S03_100", SillyGenOpStrategy_03(100, selStrat)),
    Conf("S03_110", SillyGenOpStrategy_03(110, selStrat)),
    Conf("S03_120", SillyGenOpStrategy_03(120, selStrat)))

  // Like 02 but with a wider range. The smaller range from 02 brought no
  // significant differences
  val confs_02a = List(
    Conf("S03_80", SillyGenOpStrategy_03(10, selStrat)),
    Conf("S03_90", SillyGenOpStrategy_03(50, selStrat)),
    Conf("S03_100", SillyGenOpStrategy_03(100, selStrat)),
    Conf("S03_110", SillyGenOpStrategy_03(150, selStrat)),
    Conf("S03_120", SillyGenOpStrategy_03(200, selStrat)))

  // Testing the performance of different simple mutation rates.
  // Simple mutation rate means, it does not change by generation count
  val confs_03 = List(
    Conf("S02_005", SillyGenOpStrategy_02(0.005, selStrat)),
    Conf("S02_01", SillyGenOpStrategy_02(0.01, selStrat)),
    Conf("S02_02", SillyGenOpStrategy_02(0.02, selStrat)),
    Conf("S02_05", SillyGenOpStrategy_02(0.05, selStrat)))

  // Testing the performance of different simple mutation rates without checking for duplicates.
  // Simple mutation rate means, it does not change by generation count
  val confs_04 = List(
    Conf("S02_005", SillyGenOpStrategy_04(0.005, selStrat)),
    Conf("S02_01", SillyGenOpStrategy_04(0.01, selStrat)),
    Conf("S02_02", SillyGenOpStrategy_04(0.02, selStrat)),
    Conf("S02_05", SillyGenOpStrategy_04(0.05, selStrat)))

  // Test SelectionStrategies
  val confs_05 = {
    val rSelStrat = RandomSelectionStrategy(random)
    val aSelStrat = AlphaSelectionStrategy(random)
    val agSelStrat_10 = AlphaGroupSelectionStrategy(random, 10)
    val agSelStrat_20 = AlphaGroupSelectionStrategy(random, 20)
    val gSelStrat_10 = GroupOfFittestSelectionStrategy(random, 10)
    val gSelStrat_20 = GroupOfFittestSelectionStrategy(random, 20)
    List(
      Conf("S04_R", SillyGenOpStrategy_04(0.001, rSelStrat)),
      Conf("S04_A", SillyGenOpStrategy_04(0.001, aSelStrat)),
      Conf("S04_AG_10", SillyGenOpStrategy_04(0.001, agSelStrat_10)),
      Conf("S04_AG_20", SillyGenOpStrategy_04(0.001, agSelStrat_20)),
      Conf("S04_G_10", SillyGenOpStrategy_04(0.001, gSelStrat_10)),
      Conf("S04_G_20", SillyGenOpStrategy_04(0.001, gSelStrat_20)))
  }

  // Choose a configuration list  
  val out = confs_05.par.map(c => run(c))

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
