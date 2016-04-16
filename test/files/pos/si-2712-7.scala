package test

// Original test case from,
//
//   https://issues.scala-lang.org/browse/SI-2712
object Test1 {
  class Foo[F[_], T]
  def meh[M[_[_]], F[_]](x: M[F]): M[F] = x
  meh(new Foo[List, Int]) // solves ?M = [X[_]]Foo[Int, X[_]] ?A = List ...
}
