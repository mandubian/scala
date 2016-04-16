package test


object Test6{
  trait TC[F[_]]
  trait TC2[F[_]]
  class Foo[F[_[_]], G[_[_]], A, B]
  new Foo[TC, TC2, String, Int]

  def meh[G[_[_[_]], _], A](g: G[TC, A]) = ???
  meh(new Foo[TC, TC2, String, Int]) // solves ?G = [X[_[_]], A]Foo[X, Y, String, A]
}
