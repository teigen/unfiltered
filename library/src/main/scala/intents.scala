package unfiltered

import unfiltered.request.HttpRequest
import unfiltered.response.{ResponseFunction,HttpResponse,Pass}

case class PassingIntent[A,B] (
  completed: PartialFunction[A,B]
) {
  def apply(x: A) = completed(x)
  def orElse(that: PassingIntent[A,B]) =
    fold(a => that(a), (a,b) => b)
  def fold[B1](onPass: A => B1, onMatch: (A, B) => B1) =
    PassingIntent({
      case req => completed(req) match {
        case rf if rf == Pass => onPass(req)
        case rf => onMatch(req, rf)
      }
    }: PartialFunction[A,B1]): PassingIntent[A,B1]
}

object Cycle {
  /** A rountrip intent is a set of instructions for producting
   * a complete response to a request. Plans that contain intents
   * of this type can be run against a general set of tests. */
  type PF[A,B] = PartialFunction[HttpRequest[A], ResponseFunction[B]]
  type Intent[A,B] = PassingIntent[HttpRequest[A], ResponseFunction[B]]
  /** Object to facilitate Cycle.Intent definitions. Type annotations
   *  are another option. */
  object Intent {
    implicit def apply[A,B](pf: PF[A,B]) =
      PassingIntent(pf.orElse({ case _ => Pass }: PF[A,B]))
    @deprecated("Use PassingIntent.complete()")
    def complete[A,B](pf: PF[A,B]): PF[A,B] =
      pf.orElse({ case _ => Pass })
  }
}

object Async {
  type PF[A,B] = PartialFunction[HttpRequest[A] with Responder[B], Any]
  type Intent[A,B] =
    PassingIntent[HttpRequest[A] with Responder[B], Any]
  object Intent {
    def apply[A,B](pf: PF[A,B]) =
      PassingIntent(pf.orElse({ case _ => Pass }: PF[A,B]))
  }
  trait Responder[+R] {
    def respond(rf: unfiltered.response.ResponseFunction[R])
  }
}

