package test

object Test1 {
  class Foo[F[_], T]
  def meh[M[_[_]], F[_]](x: M[F]): M[F] = x
  meh(new Foo[List, Int]) // solves ?M = [X[_]]Foo[Int, X[_]] ?A = List ...
}

object Test2 {
  trait TC[T]
  class Foo[F[_], G[_]]
  def meh[G[_[_]]](g: G[TC]) = ???
  meh(new Foo[TC, TC]) // solves ?G = [X[_]]Foo[X, TC]
}

object Test3 {
  trait TC[F[_]]
  trait TC2[F[_]]
  class Foo[F[_[_]], G[_[_]]]
  new Foo[TC, TC2]

  def meh[G[_[_[_]]]](g: G[TC]) = ???
  meh(new Foo[TC, TC2]) // solves ?G = [X[_[_]]]Foo[X, TC]
}

object Test4 {
  trait TC[T]
  trait TC2[T]
  class Foo[F[_], G[__], A, B]
  new Foo[TC, TC2, String, Int]

  def meh[G[_[_], _[_]]](g: G[TC, TC2]) = ???
  meh(new Foo[TC, TC2, String, Int]) // solves ?G = [X[_], Y[_]]Foo[X, Y, String, Int]
}

object Test5 {
  trait TC[F[_]]
  trait TC2[F[_]]
  class Foo[F[_[_]], G[_[_]], A, B]
  new Foo[TC, TC2, String, Int]

  def meh[G[_[_[_]], _[_[_]]]](g: G[TC, TC2]) = ???
  meh(new Foo[TC, TC2, String, Int]) // solves ?G = [X[_[_]], Y[_[_]]]Foo[X, Y, String, Int]
}
