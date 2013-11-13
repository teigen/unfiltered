package unfiltered.directives

import unfiltered.request._
import unfiltered.response._

import Result.{Success, Failure, Error}
import scala.annotation.implicitNotFound

trait Directives extends Monads { self =>

  object Directive {
    def apply[T, R, A](run:HttpRequest[T] => M[Result[R, A]]):Directive[T, R, A] =
      new Directive[T, R, A](run)

    trait Fail[-T, +R, +A]{
      def map[X](f:R => X):Directive[T, X, A]
      def ~> [RR, TT <: T, AA >: A](and: ResponseFunction[RR])
                                   (implicit ev: Fail[T, R, A] <:< Fail[TT, ResponseFunction[RR], AA])
      : Directive[TT, ResponseFunction[RR], AA] = ev(this).map(_ ~> and)
    }

    @implicitNotFound("implicit instance of Directive.Eq[${X}, ${V}, ?, ?, ?] not found")
    case class Eq[-X, -V, -T, +R, +A](directive:(X, V) => Directive[T, R, A])

    @implicitNotFound("implicit instance of Directive.Gt[${X}, ${V}, ?, ?, ?] not found")
    case class Gt[-X, -V, -T, +R, +A](directive:(X, V) => Directive[T, R, A])

    @implicitNotFound("implicit instance of Directive.Lt[${X}, ${V}, ?, ?, ?] not found")
    case class Lt[-X, -V, -T, +R, +A](directive:(X, V) => Directive[T, R, A])
  }

  class Directive[-T, +R, +A](run:HttpRequest[T] => M[Result[R, A]])
    extends (HttpRequest[T] => M[Result[R, A]]) {

    def apply(request: HttpRequest[T]) = run(request)

    def map[TT <: T, RR >: R, B](f:A => B):Directive[TT, RR, B] =
      Directive(r => run(r).map(_.map(f)))

    def flatMap[TT <: T, RR >: R, B](f:A => Directive[TT, RR, B]):Directive[TT, RR, B] =
      Directive(r => run(r).flatMap{
        case Success(a)   => f(a)(r)
        case Failure(res) => M.point(Failure(res))
        case Error(res)   => M.point(Error(res))
      })

    /** Doesn't filter. Scala requires something to be defined for pattern matching in for
      expressions, and we do use that. */
    def withFilter(f:A => Boolean): Directive[T, R, A] = this
    /** Doesn't filter. Scala requires something to be defined for pattern matching in for
      expressions, and we do use that. */
    def filter(f:A => Boolean): Directive[T, R, A] = this

    def orElse[TT <: T, RR >: R, B >: A](next: => Directive[TT, RR, B]): Directive[TT, RR, B] =
      Directive(r =>run(r).flatMap{
        case Success(a)   => M.point(Success(a))
        case Failure(res) => next(r)
        case Error(res)   => M.point(Error(res))
      })

    def and[TT <: T, E, B, RF](other: => Directive[TT, JoiningResponseFunction[E, RF], B])
                              (implicit ev: R <:< JoiningResponseFunction[E, RF]) =
      Directive[TT, JoiningResponseFunction[E, RF], (A, B)](req => for {
        a <- run(req)
        b <- other(req)
      } yield a and b)

    def fail: Directive.Fail[T, R, A] = new Directive.Fail[T, R, A] {
      def map[B](f: R => B) =
        Directive(r => run(r).map(_.fail.map(f)))
    }

    /* symbolic api */
    def | [TT <: T, RR >: R, B >: A](next: => Directive[TT, RR, B]): Directive[TT, RR, B] =
      orElse(next)

    def &[TT <: T, E, B, RF](other: => Directive[TT, JoiningResponseFunction[E, RF], B])
                            (implicit ev: R <:< JoiningResponseFunction[E, RF]) = this and other

    def ~[TT <: T, RR >: R, B](next:Directive[TT, RR, B]):Directive[TT, RR, A ~ B] =
      for {
        a  <- this
        b  <- next
      } yield new ~(a, b)

    def ~![TT <: T, RR >: R, B](next:Directive[TT, RR, B]):Directive[TT, RR, A ~ B] =
      for {
        a <- this
        _ <- commit
        b <- next
      } yield new ~(a, b)

    def ~>[TT <: T, RR >: R, B](next:Directive[TT, RR, B]):Directive[TT, RR, B] =
      for {
        _ <- this
        b <- next
      } yield b

    def ~>![TT <: T, RR >: R, B](next:Directive[TT, RR, B]):Directive[TT, RR, B] =
      for {
        _ <- this
        _ <- commit
        b <- next
      } yield b

    def <~[TT <: T, RR >: R](next:Directive[TT, RR, _]):Directive[TT, RR, A] =
      for {
        a <- this
        _ <- next
      } yield a

    def <~![TT <: T, RR >: R](next:Directive[TT, RR, _]):Directive[TT, RR, A] =
      for {
        a <- this
        _ <- commit
        _ <- next
      } yield a

    def >>[TT <: T, RR >: R, B](f:A => Directive[TT, RR, B]):Directive[TT, RR, B] =
      this flatMap f

    def ^^[B](f:A => B):Directive[T, R, B] =
      this map f

    def ^^^[B](f: => B):Directive[T, R, B] =
      this map { _ => f }
  }

  case class ~[+A, +B](_1:A, _2:B)
  /* end symbolc api */

  def result[R, A](r: => M[Result[R, A]]) = Directive[Any, R, A](_ => r)

  def success[A](value: => M[A]) = result[Nothing, A](value.map(Success(_)))

  def failure[R](r: => M[R]) = result[R, Nothing](r.map(Failure(_)))

  def error[R](r: => M[R]) = result[R, Nothing](r.map(Error(_)))

  object commit extends Directive[Any, Nothing, Unit](_ => M.point(Success(()))){
    override def flatMap[T, R, A](f:Unit => Directive[T, R, A]):Directive[T, R, A] =
      commit(f())

    def apply[T, R, A](d:Directive[T, R, A]) = Directive[T, R, A]{ d(_).map{
      case Failure(response) => Error(response)
      case result            => result
    }}
  }

  def getOrElse[R, A](opt:Option[A], orElse: => ResponseFunction[R]) =
    opt.map(a => pointed.success(a)).getOrElse(pointed.failure(orElse))

  /* HttpRequest has to be of type Any because of type-inference (SLS 8.5) */
  case class when[A](f:PartialFunction[HttpRequest[Any], A]){
    def orElse[R](fail:ResponseFunction[R]) =
      request[Any].flatMap(r => if(f.isDefinedAt(r)) pointed.success(f(r)) else pointed.failure(fail))
  }

  /* convenience when writing generic directives */
  object pointed {
    def apply[A](value: => A)                = M.point(value)
    def result[R, A](value: => Result[R, A]) = self.result(pointed(value))
    def success[A](value: => A)              = self.success(pointed(value))
    def failure[A](value: => A)              = self.failure(pointed(value))
    def error[A](value: => A)                = self.error(pointed(value))
  }

  /* directive versions of all methods on HttpRequest */
  object request {
    def apply[T] = Directive[T, Nothing, HttpRequest[T]](r => M.point(Success(r)))

    def underlying[T]                  = apply[T] map { _.underlying }
    def inputStream                    = apply[Any] map { _.inputStream }
    def reader                         = apply[Any] map { _.reader }
    def protocol                       = apply[Any] map { _.protocol }
    def method                         = apply[Any] map { _.method }
    def uri                            = apply[Any] map { _.uri }
    def parameterNames                 = apply[Any] map { _.parameterNames }
    def parameterValues(param: String) = apply[Any] map { _.parameterValues(param) }
    def headerNames                    = apply[Any] map { _.headerNames }
    def headers(name: String)          = apply[Any] map { _.headers(name) }
    def cookies                        = apply[Any] map { case Cookies(cookies) => cookies }
    def isSecure                       = apply[Any] map { _.isSecure }
    def remoteAddr                     = apply[Any] map { _.remoteAddr }
    def queryParams                    = apply[Any] map { case QueryParams(params) => params }
    def param[A](name: String)(f: Seq[String] => Option[A]) =
      parameterValues(name).map(f)
  }

  /* allows request without type annotation to be inferred as the directive request[Any], which is what you want */
  implicit def anyRequest(r:request.type) = request[Any]

  class OrderingOps[X](x:X){
    import Directive.{Eq, Gt, Lt}

    def ===[V, T, R, A](v:V)(implicit eq:Eq[X, V, T, R, A]) = eq.directive(x, v)
    def in[V, T, R, A](vs:Seq[V])(implicit eq:Eq[X, V, T, R, A]) = vs.map(v => eq.directive(x,v)).reduce(_ | _)

    def gt[V, T, R, A](v:V)(implicit gtd:Gt[X, V, T, R, A]) = gtd.directive(x, v)
    def > [V, T, R, A](v:V)(implicit gtd:Gt[X, V, T, R, A]) = gt(v)

    def lt[V, T, R, A](v:V)(implicit ltd:Lt[X, V, T, R, A]) = ltd.directive(x, v)
    def <[V, T, R, A](v:V)(implicit ltd:Lt[X, V, T, R, A])  = lt(v)

    def lte[V, T, R, A](v:V)(implicit lted:Lt[X, V, T, R, A], eqd:Eq[X, V, T, R, A]) =
      lted.directive(x, v) orElse eqd.directive(x, v)
    def <=[V, T, R, A](v:V)(implicit ltd:Lt[X, V, T, R, A], eqd:Eq[X, V, T, R, A]) =
      lte(v)

    def gte[V, T, R, A](v:V)(implicit gtd:Gt[X, V, T, R, A], eq:Eq[X, V, T, R, A]) =
      gtd.directive(x, v) orElse eq.directive(x, v)
    def >=[V, T, R, A](v:V)(implicit gtd:Gt[X, V, T, R, A], eq:Eq[X, V, T, R, A]) = gte(v)
  }

  implicit def orderingOps[X](x:X) = new OrderingOps[X](x)

  implicit def method(M:Method) =
    when{ case M(_) => } orElse MethodNotAllowed

  implicit def accepting(A:Accepts.Accepting) =
    when{ case A(_) => } orElse NotAcceptable

  implicit def defQueryParams(q:QueryParams.type) =
    request.queryParams

  implicit def defExtract[A](Ex:Params.Extract[Nothing, A]) =
    when{ case Params(Ex(a)) => a } orElse BadRequest
}
