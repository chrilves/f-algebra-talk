package psug201607._9_application

import cats.Functor
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Cogen}
import org.typelevel.discipline

object LaMonadeLibre {

  trait ArbitraryContainer[M[+_]] {
    implicit def arbitraryContainer[A : Arbitrary] : Arbitrary[M[A]]
  }

  trait Poly[G[_], M[_]] {
    def apply[A](fa : G[A]) : M[A]
  }

  //
  // Monade
  //

  trait Monad[M[+_]] {
    def point[B](b : B) : M[B]

    def flatten[B](mmb : M[M[B]]) : M[B]

    def map[A,B](ma : M[A] , f : A => B ) : M[B]

    final def flatMap[A,B](ma: M[A], f : A => M[B]) : M[B] = flatten(map(ma, f))

    def lois[A : Arbitrary : Cogen](implicit M : ArbitraryContainer[M]) = new discipline.Laws {

      implicit val   marbitrary : Arbitrary[M[A]]       = M.arbitraryContainer(implicitly[Arbitrary[A]])
      implicit val  mmarbitrary : Arbitrary[M[M[A]]]    = M.arbitraryContainer(marbitrary)
      implicit val mmmarbitrary : Arbitrary[M[M[M[A]]]] = M.arbitraryContainer(mmarbitrary)

      def monade = new SimpleRuleSet("monade",
        "map id"          -> forAll( (e:M[A]) => map(e, (x:A) => x) == e),
        "map.map"         -> forAll( (e:M[A], f : A => A, g : A => A) => map(map(e,f), g) == map(e, (x:A) => g(f(x)))),

        "flatten.flatten" -> forAll( (e:M[M[M[A]]])            => flatten(flatten(e)) == flatten(map(e, flatten[A]))),
        "point, flatten"  -> forAll( (e:M[A])                  => (flatten(map(e, point[A])) == e) && (flatten(point(e)) == e)),
        "flatten, f"      -> forAll( (e:M[M[A]], f : (A => A)) => flatten(map(e, (x : M[A]) => map(x, f))) == map(flatten(e), f))
      )
    }
  }


  //
  // Monade sur F[_]
  //

  trait MonadeSurF[M[+_]] extends Monad[M] {
    def elem[B](fb : F[B]) : M[B]
  }



  //
  // Monad Libre sur F[_] EN SUPPOSANT QUE F soit un foncteur!
  //

  sealed abstract class MonadeLibre[+B]
  case class Point[+B]      (b : B                ) extends MonadeLibre[B]
  case class FlattenElem[+B](e : F[MonadeLibre[B]]) extends MonadeLibre[B]


  object MonadeLibre {
    // C'est hien une monade sur F[_]!
    implicit def monadeSurF(implicit F : Functor[F]) = new MonadeSurF[MonadeLibre] {
      def point[B](b: B): MonadeLibre[B] = Point(b)

      def elem[B](fb: F[B]) : MonadeLibre[B] = FlattenElem(F.map(fb)(point))

      def flatten[B](mmb: MonadeLibre[MonadeLibre[B]]): MonadeLibre[B] = mmb match {
        case Point(b)         => b
        case FlattenElem(fmb) => FlattenElem(F.map(fmb)(flatten))
      }

      def map[A, B](ma: MonadeLibre[A], f: (A) => B): MonadeLibre[B] = ma match {
        case Point(b)         => Point(f(b))
        case FlattenElem(fmb) => FlattenElem(F.map(fmb)(map(_,f)))
      }
    }
  }

  //
  //  Unique morhisme vers chaque monad sur F[_]
  //

  def uniqueMorphisme[M[+_], A](ma : MonadeLibre[A])(implicit M : MonadeSurF[M], F : Functor[F]) : M[A] = ma match {
    case Point(b)         => M.point(b)
    case FlattenElem(fmb) => M.flatten(M.elem(F.map(fmb)(uniqueMorphisme[M,A])))
  }

  def uniqueMorphisme[M[+_], A](elem : Poly[F, M])(ma : MonadeLibre[A])(implicit M : Monad[M], F : Functor[F]) : M[A] = ma match {
    case Point(b)         => M.point(b)
    case FlattenElem(fmb) => M.flatten(elem(F.map(fmb)(uniqueMorphisme[M,A](elem))))
  }
}
