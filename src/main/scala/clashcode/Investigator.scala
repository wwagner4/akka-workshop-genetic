package clashcode

import akka.actor.{ActorRef, Props, Actor}
import akka.pattern.{ ask, pipe }
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext.Implicits._
import akka.util.Timeout


/**
 *
 * */
class Investigator(router: ActorRef) extends Actor {

  /** timer for interrogation */
  context.system.scheduler.schedule(FiniteDuration(1, TimeUnit.SECONDS), FiniteDuration(1, TimeUnit.SECONDS)) {
    self ! "now"
  }

  implicit val timeout = Timeout(FiniteDuration(1, TimeUnit.SECONDS)) // needed for `?` below

  def receive = {
    case "now" =>

      val name1 = (router ? NameRequest).map {
        case Hello(name) =>
          println(name)
          name
        case x => throw new IllegalArgumentException(x.toString)
      }

    case x => println(x)
  }

}
