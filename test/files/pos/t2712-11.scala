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

  // // type P[T] = T => T
  def typed[T](t: T): Unit = {}

  // [t](t => t)
  def meh[F[_], A](t: F[A]): F[Boolean] = ???

  val res = meh{(x: Int) => x}
  typed[Boolean => Boolean](res)

  val res1 = meh{(x: Option[Int]) => x}
  typed[Option[Boolean] => Option[Boolean]](res1)

  // [a, b]( (a, b, a) => b)
  def meh2[F[_, _], A, B](t: F[A, B]): F[Boolean, Float] = ???

  val res2 = meh2{(x: Int, y: String, z: Int) => y}
  typed[Function3[Boolean, Float, Boolean, Float]](res2)

  // not working
  // val res3 = meh2{(x: Bar[Int, String, Boolean]) => x }
  // typed[Function1[Bar[Int, Boolean, Float], Bar[Int, Boolean, Float]]](res3)
}
