package clashcode

import akka.actor.Actor
import akka.pattern.{ ask, pipe }
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext.Implicits._


/**
 *
 * */
class Investigator extends Actor {

  /** timer for interrogation */
  context.system.scheduler.schedule(FiniteDuration(1, TimeUnit.SECONDS), FiniteDuration(1, TimeUnit.SECONDS)) {
    self ! "now"
  }

  def receive = {
    case "now" =>
    case x => println(x)
  }

}
