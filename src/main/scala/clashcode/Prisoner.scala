package clashcode

import akka.actor.Actor

/**
 * Basic prisoner solution
 */
class Prisoner(name: String) extends Actor {

  //val remoteActor = context.system.actorFor("akka.tcp://application@clashcode.com:110/user/main")
  //remoteActor ! "test"

  def receive = {
    case NameRequest =>
      //println("asked for name")
      sender ! Hello(name)
    case PrisonerRequest(other) => sender ! PrisonerResponse(true)
    case x : PrisonerResult => println(x)
    case x : String => println(x)
  }

}
