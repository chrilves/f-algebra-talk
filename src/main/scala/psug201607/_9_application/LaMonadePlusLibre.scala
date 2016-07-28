package psug201607._9_application

import org.scalacheck.{Arbitrary, Cogen}
import org.typelevel.discipline
import org.scalacheck.Prop._
import LaMonadeLibre.{ArbitraryContainer,Poly}

object LaMonadePlusLibre {

  //
  // Monade
  //


  trait Monad[M[+_]] {
    def point[B](b : B) : M[B]

    def flatMap[A,B](ma : M[A] , f : A => M[B]) : M[B]

    final def map[A,B](ma : M[A], f : A => B) : M[B] = flatMap(ma, (x:A) => point(f(x)))

    final def flatten[A](mma : M[M[A]]) : M[A] = flatMap(mma, (x:M[A]) => x)

    def lois[A : Arbitrary : Cogen](implicit M : ArbitraryContainer[M]) = new discipline.Laws {
      import M.arbitraryContainer

      def monade = new SimpleRuleSet("monade",
        "id gauche" -> forAll( (a:A, f : (A => M[A])) => flatMap(point(a) , f) == f(a) ),
        "id droite" -> forAll( (e:M[A]) => flatMap(e, point[A]) == e),
        "assoc"     -> forAll( (e:M[A], f : (A => M[A]), g : (A => M[A])) => flatMap(flatMap(e, f), g) == flatMap(e, ((x:A) => flatMap(f(x),g))))
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
  // Monade plus libre sur F[_]
  //



  sealed abstract class MonadePlusLibre[+B]
  case class Point[+B](b : B)                                     extends MonadePlusLibre[B]
  case class FlatMap[A,+B](e : F[A], f : A => MonadePlusLibre[B]) extends MonadePlusLibre[B] {
    type inner = A
  }


  object MonadePlusLibre {
    // C'est bien une monade sur F[_] !
    implicit final val monadeSurF = new MonadeSurF[MonadePlusLibre] {
      def point[B](b : B) : MonadePlusLibre[B] = Point(b)

      def flatMap[A, B](ma : MonadePlusLibre[A], f : A => MonadePlusLibre[B]) : MonadePlusLibre[B] = ma match {
        case Point(a)        => f(a)
        case fm@FlatMap(e,g) => FlatMap(e, ((x : fm.inner) => flatMap(g(x), f)))
      }

      def elem[B](fb: F[B]): MonadePlusLibre[B] = FlatMap(fb, Point[B] _)
    }
  }

  //
  //  Unique morhisme vers chaque monad sur F[_]
  //

  def uniqueMorphisme[M[+_], A](ml : MonadePlusLibre[A])(implicit M : MonadeSurF[M]) : M[A] = ml match {
    case Point(a)        => M.point(a)
    case fm@FlatMap(e,f) => M.flatMap(M.elem(e), (x:fm.inner) => uniqueMorphisme[M, A](f(x)))
  }

  def uniqueMorphisme[M[+_], A](elem : Poly[F, M])(ml : MonadePlusLibre[A])(implicit M : Monad[M]) : M[A] = ml match {
    case Point(a)        => M.point(a)
    case fm@FlatMap(e,f) => M.flatMap(elem(e), (x:fm.inner) => uniqueMorphisme[M, A](elem)(f(x)))
  }
}
