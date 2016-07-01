package psug201607._5_morphisme

import cats.Functor
import org.scalacheck.{Arbitrary}
import org.scalacheck.Prop._
import org.typelevel.discipline

object Morphisme {

  type Algebre[Op[+ _], A] = Op[A] => A
  type CoAlgebre[Op[+ _], A] = A => Op[A]

  trait FunctorCheck[F[_]] extends Functor[F] {
    implicit def arbitrary[A](implicit A: Arbitrary[A]): Arbitrary[F[A]]
  }


  // Morphisme Général

  //
  // Morphisme Algebre
  //

  abstract class MorphismeAlgebre[Op[+ _], A, B](implicit A: Algebre[Op, A], B: Algebre[Op, B], Op: FunctorCheck[Op]) extends (A => B) {self =>

    import cats.syntax.functor._

    class Lois(implicit ArbA: Arbitrary[A]) extends discipline.Laws {

      import Op.arbitrary

      def morphisme = new SimpleRuleSet("morphisme d'algebre",
        "morphisme" -> forAll((opa: Op[A]) => self(A(opa)) == B(opa.map(self)))
      )
    }

    def lois(implicit ArbA: Arbitrary[A]) = new Lois
  }

  object MorphismeAlgebre {
    def apply[Op[+ _], A, B](f: A => B)(implicit A: Algebre[Op, A], B: Algebre[Op, B], Op: FunctorCheck[Op]) = new MorphismeAlgebre[Op, A, B] {
      def apply(a: A): B = f(a)
    }
  }

  //
  // IsoMorphisme AlgebrePSUG
  //


  abstract class IsoMorphismeAlgebre[Op[+ _], A, B](implicit A: Algebre[Op,A], B: Algebre[Op,B], Op: FunctorCheck[Op]) extends MorphismeAlgebre[Op, A, B] {self =>

    def inverse: IsoMorphismeAlgebre[Op, B, A]

    class Lois(implicit ArbA: Arbitrary[A], ArbB: Arbitrary[B]) extends super.Lois {
      // Identique à morphisme2, en explicitant chaque operation
      def isoMorphisme = new RuleSet {
        val name    = "isoMorphisme Algebre"
        val parents = Seq(morphisme)
        val bases   = Seq("inverse" -> inverse.lois.morphisme)
        val props   = Seq(
          "idA" -> forAll((a: A) => inverse(self(a)) == a),
          "idB" -> forAll((b: B) => self(inverse(b)) == b)
        )
      }
    }

    def lois(implicit ArbA: Arbitrary[A], ArbB: Arbitrary[B]) = new Lois
  }

  object IsoMorphismeAlgebre {
    def apply[Op[+ _], A, B](f: A => B, g: B => A)(implicit A: Algebre[Op,A], B: Algebre[Op,B], Op: FunctorCheck[Op]) = new IsoMorphismeAlgebre[Op, A, B] {self =>
      def apply(a: A): B = f(a)

      def inverse = new IsoMorphismeAlgebre[Op, B, A] {
        def apply(b: B): A = g(b)

        def inverse: IsoMorphismeAlgebre[Op, A, B] = self
      }
    }
  }

  //
  // Morphisme CoAlgebre
  //


  abstract class MorphismeCoAlgebre[Op[+ _], A, B](implicit A: CoAlgebre[Op, A], B: CoAlgebre[Op, B], Op: FunctorCheck[Op]) extends (A => B) {self =>

    import cats.syntax.functor._

    class Lois(implicit ArbA: Arbitrary[A]) extends discipline.Laws {

      def morphisme = new SimpleRuleSet("morphisme de co algebre",
        "morphisme" -> forAll((a: A) => A(a).map(self) == B(self(a)))
      )
    }

    def lois(implicit ArbA: Arbitrary[A]) = new Lois
  }

  object MorphismeCoAlgebre {
    def apply[Op[+ _], A, B](f: A => B)(implicit A: CoAlgebre[Op, A], B: CoAlgebre[Op, B], Op: FunctorCheck[Op]) = new MorphismeCoAlgebre[Op, A, B] {
      def apply(a: A): B = f(a)
    }
  }

  //
  // IsoMorphisme AlgebrePSUG
  //


  abstract class IsoMorphismeCoAlgebre[Op[+ _], A, B](implicit A: CoAlgebre[Op,A], B: CoAlgebre[Op,B], Op: FunctorCheck[Op]) extends MorphismeCoAlgebre[Op, A, B] {self =>

    def inverse: IsoMorphismeCoAlgebre[Op, B, A]

    class Lois(implicit ArbA: Arbitrary[A], ArbB: Arbitrary[B]) extends super.Lois {
      // Identique à morphisme2, en explicitant chaque operation
      def isoMorphisme = new RuleSet {
        val name    = "isoMorphisme CoAlgebre"
        val parents = Seq(morphisme)
        val bases   = Seq("inverse" -> inverse.lois.morphisme)
        val props   = Seq(
          "idA" -> forAll((a: A) => inverse(self(a)) == a),
          "idB" -> forAll((b: B) => self(inverse(b)) == b)
        )
      }
    }

    def lois(implicit ArbA: Arbitrary[A], ArbB: Arbitrary[B]) = new Lois
  }

  object IsoMorphismeCoAlgebre {
    def apply[Op[+ _], A, B](f: A => B, g: B => A)(implicit A: CoAlgebre[Op,A], B: CoAlgebre[Op,B], Op: FunctorCheck[Op]) = new IsoMorphismeCoAlgebre[Op, A, B] {self =>
      def apply(a: A): B = f(a)

      def inverse = new IsoMorphismeCoAlgebre[Op, B, A] {
        def apply(b: B): A = g(b)

        def inverse: IsoMorphismeCoAlgebre[Op, A, B] = self
      }
    }
  }

}