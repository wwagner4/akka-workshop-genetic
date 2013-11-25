package clashcode

import akka.actor.{Props, ActorSystem}
import akka.routing.{RandomRouter, ConsistentHashingRouter, FromConfig}
import akka.cluster.routing.{ClusterRouterSettings, ClusterRouterConfig}
import clashcode.robot._
import java.io.{ObjectOutputStream, ByteArrayOutputStream}

object Main extends App {



  override def main(args: Array[String]) {

    var max = 0;
    (0 until 100).par.foreach(i => {
      val entry = Situations.getRandomEntry
      val candidate = entry.toCandidate
      val value = Evaluator.evaluate(candidate)
      if (value > max) max = value
    })
    println(max)

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

}