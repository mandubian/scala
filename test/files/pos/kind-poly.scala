object Test {
  // def foo[F <: KindPolymorphic]: F = null.asInstanceOf[F]

  // val i = foo[Int]        // OK
  // val l = foo[List[Int]]  // OK
  // val l = foo[List]        // KO but error is `error:null` :D

  // Basic Kind polymorphism sample
  trait Foo[T <: KindPolymorphic] { type Out ; def id(t: Out): Out = t }

  object Foo {
    implicit def foo0[T] = new Foo[T] { type Out = T }
    implicit def foo1[T[_]] = new Foo[T] { type Out = T[Any] }
    implicit def foo2[T[_, _]] = new Foo[T] { type Out = T[Any, Any] }
  }

  def foo[T <: KindPolymorphic](implicit f: Foo[T]): f.type = f
  foo[Int].id(23)
  foo[List].id(List[Any](1, 2, 3))
  foo[Map].id(Map[Any, Any](1 -> "toto", 2 -> "tata", 3 -> "tutu"))

  // Now let's create a kind-polymorphic List
  
  // Classic Heterogenous List to unapply kind polymorphic params
  sealed trait HList
  final case class ::[+H, +T <: HList](head : H, tail : T) extends HList
  sealed trait HNil extends HList
  final case object HNil extends HNil

  // The Kind Polymorphic List
  sealed trait KPList

  sealed trait KPNil extends KPList
  case object KPNil extends KPNil
  
  sealed trait :::[H <: KindPolymorphic, T <: KPList] extends KPList
  trait KPCons[M <: KindPolymorphic, T <: KPList] extends :::[M, T]  {
    type HL <: HList
    type H
    def head: H
    def tail: T
  }

  object KPCons {
    type Aux[M <: KindPolymorphic, T <: KPList, H0, HL0 <: HList] = KPCons[M, T] { type H = H0; type HL = HL0 }
    // Polymorphic 
    trait Apply[M <: KindPolymorphic, A <: HList] { type Out }
    object Apply {
      type Aux[M <: KindPolymorphic, A <: HList, Out0] = Apply[M, A] { type Out = Out0 }
      implicit def apply0[M]: Aux[M, HNil, M] = new Apply[M, HNil] { type Out = M }
      implicit def apply1[M[_], A]: Aux[M, A :: HNil, M[A]] = new Apply[M, A :: HNil] { type Out = M[A] }
      implicit def apply2[M[_, _], A, B]: Aux[M, A :: B :: HNil, M[A, B]] = new Apply[M, A :: B :: HNil] { type Out = M[A, B] }
    }

    trait Unapply[M <: KindPolymorphic, O] { type Out <: HList }
    object Unapply {
      type Aux[M <: KindPolymorphic, O, Out0 <: HList] = Unapply[M, O] { type Out = Out0 }

      implicit def unapply0[M]: Aux[M, M, HNil] = new Unapply[M, M] { type Out = HNil }
      implicit def unapply1[M[_], A0]: Unapply.Aux[M, M[A0], A0 :: HNil] = new Unapply[M, M[A0]] { type Out = A0 :: HNil }
      implicit def unapply2[M[_, _], A0, B0]: Aux[M, M[A0, B0], A0 :: B0 :: HNil] = new Unapply[M, M[A0, B0]] { type Out = A0 :: B0 :: HNil }
    }

    // the list builder
    trait KPConsBuilder[M <: KindPolymorphic] {
      def apply[H0, HL0 <: HList, T <: KPList](head0: H0, tail0: T)(implicit unap: Unapply.Aux[M, H0, HL0]): KPCons.Aux[M, T, H0, HL0] = new KPCons[M, T] {
        type HL = HL0
        type H = H0
        val head: H = head0
        val tail: T = tail0
      }
    }

    def apply[M <: KindPolymorphic] = new KPConsBuilder[M] {}
  }

  case class Bar[A](a: A)

  // Let's create some kind-polymorphic list
  val kl = 
    KPCons[Bar](
      Bar(5)
    , KPCons[String](
        "toto"
      , KPCons[List](
          List(1, 2, 3)
        , KPCons[Map](
            Map("toto" -> 1L, "tata" -> 2L)
          , KPNil
          )
        )
      )
    )

  val h: Bar[Int] = kl.head

}
