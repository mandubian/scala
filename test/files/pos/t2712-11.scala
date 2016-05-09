package test

object HK {
  sealed trait Coproduct
  @scala.annotation.unifyRightToLeft
  sealed trait :+:[+H, +T <: Coproduct] extends Coproduct
  sealed trait CNil extends Coproduct

  trait NaturalTransformation[F[_], G[_]]
  type ~>[F[_], G[_]] = NaturalTransformation[F, G]

  def foo[F[_], G[_]](f: F ~> G): Unit = ???

  type Id[A] = A

  trait Bar[A, B, C]
  val a: ({type λ[T] = Bar[Int, String, T] })#λ ~> Id = ???
  foo(a)

  val b: ({type λ[T] = T :+: CNil })#λ ~> Id = ???
  foo(b)

  val c : ({type λ[T] = Option[T] :+: String :+: List[T] :+: Bar[Int, String, T] :+: CNil })#λ ~> Id = ???
  foo(c)

  def foo2[F[_], A](t: F[A]): Unit = ???
  foo2((List(4), "toto", new Bar[Boolean, String, Int] {}))
  val d: (Option[Int], List[Int], String, Bar[Boolean, String, Int]) = (Some(5), List(4), "toto", new Bar[Boolean, String, Int] {})
  foo2(d)
}
