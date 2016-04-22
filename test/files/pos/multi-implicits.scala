package test

object Test {
  // A couple of type classes with type members ...
  trait Foo[T] {
    type A
  }

  object Foo {
    implicit val fooIS = new Foo[Int] { type A = String }
  }

  trait Bar[T] {
    type B
    val value: B
  }

  object Bar {
    implicit val barSB = new Bar[String] {
      type B = Boolean
      val value = true
    }
  }

  trait Baz[T]
  object Baz {
    implicit def baz[T]: Baz[T] = new Baz[T] {}
  }

  def run[T: Baz](t: T)(implicit foo: Foo[T])(implicit bar: Bar[foo.A]): bar.B = bar.value

  val value = run(23)
  assert(value: Boolean)

  val value2 = run(23)(Baz.baz, Foo.fooIS)(Bar.barSB)
  assert(value: Boolean)
}
