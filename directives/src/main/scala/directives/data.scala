package unfiltered.directives

import scala.util.control.Exception._

trait Data extends Directives{

  def data:data

  trait data {
    trait Interpreter[A, B, +E] { self =>

      def interpret(a: A, name: String): Either[E, B]

      def ~> [C, EE >: E](implicit next: Interpreter[B, C, EE]): Interpreter[A, C, EE] =
        new Interpreter[A, C, EE] {
          def interpret(a: A, name: String): Either[EE, C] =
            self.interpret(a, name).right.flatMap {
              r => next.interpret(r, name)
            }
        }

      /** Lifts an Interpreter into a Directive that interprets a named request parameter */
      def named[EE >: E](name: String)(implicit to: Interpreter[Seq[String], A, EE]):Directive[Any, EE, B] =
        request.parameterValues(name).flatMap{ seq =>
          pointed.result(to.interpret(seq, name).right.flatMap{ r =>
            self.interpret(r, name)
          }.fold(Result.Failure(_), Result.Success(_)))
        }

      /** Lifts an Interpreter into a Directive that interprets a provided value */
      def named[EE >: E](name: String, value: => A):Directive[Any, EE, B] =
        pointed.result(self.interpret(value, name).fold(Result.Failure(_), Result.Success(_)))
    }

    object Interpreter {
      implicit def identity[A]:Interpreter[A, A, Nothing] = new Interpreter[A, A, Nothing] {
        def interpret(seq: A, name: String) = Right(seq)
      }

      implicit def defInterpreterString = as.String

      def apply[A,B](f: A => B) = new Interpreter[A, B, Nothing] {
        def interpret(a: A, name: String) = Right(f(a))
      }
    }

    case class Fallible[A, B](cf: A => Option[B])
      extends Interpreter[Option[A], Option[B], Nothing] {
      def interpret(opt: Option[A], name: String) =
        Right(opt.flatMap(cf))
      def fail[E](handle: (String, A) => E) =
        new Strict(cf, handle)
    }

    class Strict[A, B, +E](cf: A => Option[B], handle: (String, A) => E)
      extends Interpreter[Option[A], Option[B], E] {
      def interpret(option: Option[A], name: String): Either[E, Option[B]] =
        option.map { a =>
          cf(a).map(Some(_)).toRight(handle(name, a))
        }.getOrElse(Right(None))
    }
    object Conditional {
      def apply[A](f: A => Boolean) = Fallible[A,A]( a => Some(a).filter(f))
    }

    object Requiring {
      def apply[A] = new RequireBuilder[A]
    }
    class Requiring[A, +E](handle: String => E)
      extends Interpreter[Option[A], A, E] {
      def interpret(option: Option[A], name: String): Either[E, A] =
        option.toRight(handle(name))
    }
    class RequireBuilder[A] {
      def fail[E](handle: String => E) =
        new Requiring[A,E](handle)
    }

    object as {
      object Int extends Fallible[String,Int](s => allCatch.opt { s.toInt })

      object Long extends Fallible[String,Long](s => allCatch.opt { s.toLong })

      object BigInt extends Fallible[String,BigInt](s =>
        allCatch.opt { math.BigInt(s) }
      )

      object Float extends Fallible[String,Float](s => allCatch.opt { s.toFloat })

      object Double extends Fallible[String,Double](s => allCatch.opt { s.toDouble })

      object BigDecimal extends Fallible[String,BigDecimal](s =>
        allCatch.opt { new java.math.BigDecimal(s) }
      )

      object String extends Interpreter[Seq[String], Option[String], Nothing] {
        def interpret(seq: Seq[String], name: String) = Right(seq.headOption)

        val trimmed = Interpreter[Option[String],Option[String]]( opt => opt.map { _.trim } )
        val nonEmpty = Conditional[String]( _.nonEmpty )
      }

      object Option {
        def apply[T] = new FallibleImplicit[T]
      }

      object Required {
        def apply[T] = new RequiredImplicit[T]
      }

      /** Bridge class for finding an implicit As of a parameter type T */
      class FallibleImplicit[T] {
        def named[E](name: String)
                    (implicit to: Interpreter[Seq[String],Option[T],E])
        : Directive[Any,E,Option[T]] =
          to named name
      }

      class RequiredImplicit[T] {
        def named[E](name: String)
                    (implicit to: Interpreter[Seq[String],Option[T],E],
                     req: Interpreter[Option[T],T,E])
        : Directive[Any,E,T] =
          (to ~> req) named name
      }
    }
  }
}


