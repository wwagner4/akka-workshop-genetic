package clashcode

import akka.actor.Actor

/**
 * Basic prisoner solution
 */
class Prisoner(name: String) extends Actor {

  val remoteActor = context.system.actorFor("akka.tcp://application@10.200.1.27:110/user/main")
  remoteActor ! Hello(name)

  def receive = {
    case _ : NameRequest =>
      //println("asked for name")
      sender ! Hello(name)
    case PrisonerRequest(other) => sender ! PrisonerResponse(true)
    case x => println(x)
  }

}
