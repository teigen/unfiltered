package unfiltered.directives

trait Monads {
  type M[+A]

  def M:Monad

  trait Monad{
    def point[A](value: => A):M[A]
    def bind[A, B](m:M[A])(f:A => M[B]):M[B]
    def map[A, B](m:M[A])(f:A => B):M[B] = bind(m)(a => point(f(a)))
  }

  /* support for map/flatMap/for-comprehensions, so we can avoid the inconvenient M.bind and M.map */
  class MSyntax[A](m:M[A]){
    def map[B](f:A => B) = M.map(m)(f)
    def flatMap[B](f:A => M[B]):M[B] = M.bind(m)(f)
  }

  implicit def mSyntax[A](m:M[A]) = new MSyntax(m)
}