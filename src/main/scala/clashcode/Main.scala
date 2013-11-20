package clashcode

import akka.actor.{Props, ActorSystem}
import akka.routing.{RandomRouter, ConsistentHashingRouter, FromConfig}
import akka.cluster.routing.{ClusterRouterSettings, ClusterRouterConfig}

object Main extends App {

  override def main(args: Array[String]) {

    val system = ActorSystem("cluster")
    system.actorOf(Props(classOf[Prisoner], "PrisonerA"), "player")
    system.actorOf(Props(classOf[Prisoner], "PrisonerB"), "prisonerB")
    system.actorOf(Props(classOf[Prisoner], "PrisonerC"), "player2")

    val router = system.actorOf(Props.empty.withRouter(
      ClusterRouterConfig(
        RandomRouter(),
        ClusterRouterSettings(totalInstances = 100, routeesPath = "/user/player", allowLocalRoutees = true, useRole = None))),
      name = "router")

    system.actorOf(Props(classOf[Investigator], router), "inv")

    readLine()
    system.shutdown()
    //system2.shutdown()
  }

}