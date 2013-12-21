package clashcode

import akka.actor.{ Props, ActorSystem }
import akka.routing.{ RandomRouter, ConsistentHashingRouter, FromConfig }
import akka.cluster.routing.{ ClusterRouterSettings, ClusterRouterConfig }
import clashcode.robot._
import java.io.{ FileOutputStream, ObjectOutputStream, ByteArrayOutputStream }

object MainWW extends App {

  val code = "55355322353532311311311354322344234441130030032230303231131131134444444444444444444444444544445344113444444444444444444444444444"
  val ev = new Evolution(200, code)
  val start = System.currentTimeMillis
  (0 until 30000).foreach {
    i =>
      {
        ev.tick(20)
        ev.debug()
        save("best.txt", ev.candidates.head.code.bits)
      }
  }
  val done = System.currentTimeMillis - start
  println(done)

  def save(name: String, array: Seq[Byte]) {
    val o = new FileOutputStream("best.txt")
    o.write(array.mkString.getBytes)
    o.close()
  }

}