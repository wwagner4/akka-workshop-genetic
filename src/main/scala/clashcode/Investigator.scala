package clashcode

import akka.actor._
import akka.pattern.{ ask, pipe }
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit
import scala.concurrent.ExecutionContext.Implicits._
import akka.util.Timeout
import akka.actor.ActorIdentity


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

      val x = for {
        ref1 <- (router ? Identify("x")).mapTo[ActorIdentity]
        ref2 <- (router ? Identify("x")).mapTo[ActorIdentity]
      } yield (ref1, ref2)

      val y = for {
        (ref1, ref2) <- x
        hello1 <- (ref1.getRef ? NameRequest).mapTo[Hello]
        hello2 <- (ref2.getRef  ? NameRequest).mapTo[Hello]
      } yield(hello1, hello2)

      y.foreach(hello => println(hello))

    case x => println(x)
  }

}
