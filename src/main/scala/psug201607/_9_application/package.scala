package psug201607

import org.scalacheck.{Arbitrary, Cogen, Gen}


package object _9_application {
  implicit def arbitraryFunction[A,B](implicit A : Cogen[A], B : Arbitrary[B]) : Arbitrary[A => B] = Arbitrary(Gen.function1[A,B](B.arbitrary))

  type F[X] = (X,X)
}
