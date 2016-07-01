package psug201607

import org.scalacheck.{Arbitrary, Gen}


object Lib {

   implicit def arbitraryAny : Arbitrary[Any] = Arbitrary(implicitly[Arbitrary[String]].arbitrary.map(x => (x : Any)))

  implicit def arbitraryPair[T,U](implicit T : Arbitrary[T], U : Arbitrary[U]) : Arbitrary[(T,U)] = Arbitrary {
    for {
      t <- T.arbitrary
      u <- U.arbitrary
    } yield (t,u)
  }

}
