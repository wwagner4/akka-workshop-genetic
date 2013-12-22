package clashcode

import java.io.FileOutputStream
import clashcode.robot.CandidateCode
import clashcode.robot.Evolution
import clashcode.robot.Situations
import java.text.SimpleDateFormat
import java.util.Date

object Main extends App {

  val ts: String = createTimestamp
  
  val ev = new Evolution(RandomCandidates(200))
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

trait CandidateCodeFactory {

  def createCodes: Seq[CandidateCode]

}

case class RandomCandidates(poolSize: Int) extends CandidateCodeFactory {

  def createCodes: Seq[CandidateCode] = {
    (1 to poolSize).map(_ => Situations.getRandomCode)
  }

}