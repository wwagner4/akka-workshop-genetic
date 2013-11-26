package clashcode

import akka.actor.{Props, ActorSystem}
import akka.routing.{RandomRouter, ConsistentHashingRouter, FromConfig}
import akka.cluster.routing.{ClusterRouterSettings, ClusterRouterConfig}
import clashcode.robot._
import java.io.{FileOutputStream, ObjectOutputStream, ByteArrayOutputStream}

object Main extends App {



  override def main(args: Array[String]) {

    /*
    val situationMapping = Situations.all.map(s =>
      s.top.id * 3 * 3 * 3 * 3 + s.right.id * 3 * 3 * 3 + s.bottom.id * 3 * 3 + s.left.id * 3 + (if (s.canPickup) 2 else 0))
    println(situationMapping.mkString(","))
    return;
    */

    println("bytes: " + getBytes(Situations.getRandomCode).length)

    val code = "51301022333553210300024321221420321151130104310133053320103442104444444444452424144444144444454425443445404532040441244414315453"
    val ev = new Evolution(200, None)
    println(ev.tick(10).points)

    //readLine()
    val start = System.currentTimeMillis
    (0 until 30000).foreach {
      i => {
        ev.tick(20)
        ev.debug()
        save("best.txt", ev.candidates.head.code.bits)
      }
    }
    val done = System.currentTimeMillis - start
    println(done)

    /*
    val system = ActorSystem("cluster")
    system.actorOf(Props(classOf[Prisoner], "PrisonerA"), "player")

    val router = system.actorOf(Props.empty.withRouter(
      ClusterRouterConfig(
        RandomRouter(),
        ClusterRouterSettings(totalInstances = 100, routeesPath = "/user/player", allowLocalRoutees = true, useRole = None))),
      name = "router")

    system.actorOf(Props(classOf[Investigator], router), "inv")

    readLine()
    system.shutdown()
    */
  }

  def getBytes(obj: Any) = {
    val baos = new ByteArrayOutputStream(1024)
    val o = new ObjectOutputStream(baos)
    o.writeObject(obj)
    baos.toByteArray
  }

  def save(name: String, array: Seq[Byte]) {
    val o = new FileOutputStream("best.txt")
    o.write(array.mkString.getBytes)
    o.close()
  }

}