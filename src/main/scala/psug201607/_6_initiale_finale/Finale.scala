package psug201607._6_initiale_finale

import org.scalacheck.Prop._
import org.scalacheck.Arbitrary
import org.typelevel.discipline
import psug201607._4_algebre.Algebre.{OperationPSUG, Plus, Zero}

object Finale {

  type CoAlgebrePSUG[A] = A => OperationPSUG[A]

  //
  // Morphisme de CoAlgebrePsug
  //

  abstract class MorphismeCoAlgebrePSUG[A,B](implicit A : CoAlgebrePSUG[A], B : CoAlgebrePSUG[B]) extends (A => B) { self =>

    class Lois(implicit ArbA : Arbitrary[A]) extends discipline.Laws {
      // Identique a morphisme1
      def morphisme = new SimpleRuleSet("morphisme de co algebre PSUG",
        "morphisme" -> forAll((a:A) => A(a).map(self) == B(self(a)))
      )
    }

    def lois(implicit ArbA : Arbitrary[A]) = new Lois
  }

  object MorphismeCoAlgebrePSUG {
    def apply[A,B](f : A => B)(implicit A : CoAlgebrePSUG[A], B : CoAlgebrePSUG[B]) = new MorphismeCoAlgebrePSUG[A,B] {
      def apply(a: A): B = f(a)
    }
  }

  //
  // CoAlgebre Finale
  //

  trait FinalCoPSUG {
    def matcher : OperationPSUG[FinalCoPSUG]
  }

  object FinalCoPSUG {
    implicit final val coAlgebrePSUG = new CoAlgebrePSUG[FinalCoPSUG] {
      def apply(v: FinalCoPSUG): OperationPSUG[FinalCoPSUG] = v.matcher
    }
  }

  val ZeroI                               = new FinalCoPSUG { def matcher = Zero }
  def PlusI(g:FinalCoPSUG, d:FinalCoPSUG) = new FinalCoPSUG { def matcher = Plus(g,d) }

  def infini : FinalCoPSUG = new FinalCoPSUG {
    def matcher = Plus(infini, infini)
  }

  // Unique morphisme

  def uniqueMorphisme[A](implicit A : CoAlgebrePSUG[A]) : MorphismeCoAlgebrePSUG[A, FinalCoPSUG] =
    MorphismeCoAlgebrePSUG[A, FinalCoPSUG] { (a : A) => new FinalCoPSUG {
      def matcher: OperationPSUG[FinalCoPSUG] = A(a).map(uniqueMorphisme[A].apply _)
    }}

  def unfold[A](a:A)(decompose : A => OperationPSUG[A]) : FinalCoPSUG = uniqueMorphisme(decompose)(a)
}
