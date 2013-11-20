package clashcode

import akka.actor.{Props, ActorSystem}

object Main extends App {

  override def main(args: Array[String]) {

    val system = ActorSystem("LocalSystem")
    system.actorOf(Props(classOf[Prisoner], "PrisonerA"), "prisonerA")
    system.actorOf(Props(classOf[Prisoner], "PrisonerB"), "prisonerB")
    system.actorOf(Props(classOf[Prisoner], "PrisonerC"), "prisonerC")

    readLine()
    system.shutdown()
  }

}