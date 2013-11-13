package unfiltered.directives

import unfiltered.{Async, Cycle}
import unfiltered.request.HttpRequest
import unfiltered.response.{InternalServerError, ResponseFunction}

object cycle extends cycle

/* unfiltered.Cycle.Intent directives */
trait cycle extends Global {

  type M[+A] = A

  object M extends Monad {
    def point[A](value: => A) = value
    def bind[A, B](m: A)(f: (A) => B) = f(m)
  }

  type Intent[-A, -B] = Cycle.Intent[A, B]

  object Intent extends IntentLike {
    def apply[A, B](intent: PartialFunction[HttpRequest[A], HttpRequest[A] => Result[ResponseFunction[B], ResponseFunction[B]]]):Cycle.Intent[A, B] = {
      case req if intent.isDefinedAt(req) =>
        Result.merge(intent(req)(req))
    }
  }
}

object async extends async {
  def executionContext = concurrent.ExecutionContext.global
}

/* unfiltered.Async.Intent directives */
trait async extends Global {

  implicit def executionContext:concurrent.ExecutionContext

  type M[+A] = concurrent.Future[A]

  object M extends Monad {
    def point[A](value: => A) = concurrent.Future(value)
    def bind[A, B](m: concurrent.Future[A])(f: (A) => concurrent.Future[B]) = m.flatMap(f)
  }

  type Intent[-A, -B] = Async.Intent[A, B]

  object Intent extends IntentLike {

    def apply[A, B](intent: PartialFunction[HttpRequest[A], HttpRequest[A] => concurrent.Future[Result[ResponseFunction[B], ResponseFunction[B]]]]):Async.Intent[A, B] = {
      case req if intent.isDefinedAt(req) =>
        intent(req)(req).map(Result.merge).onComplete{
          case util.Success(response) => req.respond(response)
          /* if you want to do error handling, use Future.recover before getting this far */
          case util.Failure(_)        => req.respond(InternalServerError)
        }
    }
  }
}

/* intent mapping */
trait Intents extends Monads {
  type Intent[-A, -B]

  def Intent:IntentLike

  trait IntentLike { like =>
    def apply[A, B](intent:PartialFunction[HttpRequest[A], HttpRequest[A] => M[Result[ResponseFunction[B], ResponseFunction[B]]]]):Intent[A, B]

    /** Directive intent constructor for a partial function of path strings  */
    def Path[T] = Mapping(unfiltered.request.Path[T])

    case class Mapping[T, X](from: HttpRequest[T] => X) {
      def apply[TT <: T, R](intent: PartialFunction[X, HttpRequest[TT] => M[Result[ResponseFunction[R],ResponseFunction[R]]]]): Intent[TT, R] =
        like {
          case req if intent.isDefinedAt(from(req)) => intent(from(req))
        }
    }
  }
}

/* Mmmm, cake */
trait Global extends Directives with Intents with Data {
  object data extends data
}