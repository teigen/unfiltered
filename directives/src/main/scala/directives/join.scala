package unfiltered.directives

import unfiltered.response.{ResponseFunction, HttpResponse}

class JoiningResponseFunction[E,A]
(val elements: List[E], toResponseFunction: Seq[E] => ResponseFunction[A])
  extends ResponseFunction[A] {
  def apply[B <: A](res: HttpResponse[B]) =
    toResponseFunction(elements)(res)

  def join(next: JoiningResponseFunction[E,A]) =
    new JoiningResponseFunction[E,A](elements ::: next.elements, toResponseFunction)
}

/** Convenience class, extend for a JoiningResponseFunction subclass */
class ResponseJoiner[E,A](element: E)(toResponseFunction: Seq[E] => ResponseFunction[A])
  extends JoiningResponseFunction[E,A](element :: Nil, toResponseFunction)