package psug201607._6_initiale_finale

import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.typelevel.discipline
import psug201607._4_algebre.Algebre.{AlgebrePSUG, OperationPSUG, Plus, Zero}
import org.scalacheck.Shapeless._

object Initiale {



  //
  // Morphisme AlgebrePsug
  //

  abstract class MorphismeAlgebrePSUG[A,B](implicit A : AlgebrePSUG[A], B : AlgebrePSUG[B]) extends (A => B) { self =>

    class Lois(implicit ArbA : Arbitrary[A]) extends discipline.Laws {
      // Identique à morphisme2, en explicitant chaque operation
      def morphisme1 = new SimpleRuleSet("morphisme d'algebre PSUG",
        "zero" -> Prop(self(A.zero) == B.zero),
        "plus" -> forAll((x:A,y:A) => self(A.plus(x,y)) == B.plus(self(x), self(y)))
      )

      // Identique a morphisme1
      def morphisme2 = new SimpleRuleSet("morphisme d'algebre PSUG",
        "morphisme" -> forAll((opa : OperationPSUG[A]) => self(A(opa)) == B(opa.map(self)))
      )
    }

    def lois(implicit ArbA : Arbitrary[A]) = new Lois
  }

  object MorphismeAlgebrePSUG {
    def apply[A,B](f : A => B)(implicit A : AlgebrePSUG[A], B : AlgebrePSUG[B]) = new MorphismeAlgebrePSUG[A,B] {
      def apply(a: A): B = f(a)
    }
  }




  //
  // IsoMorphisme AlgebrePSUG
  //


  abstract class IsoMorphismeAlgebrePSUG[A,B](implicit A : AlgebrePSUG[A], B : AlgebrePSUG[B]) extends MorphismeAlgebrePSUG[A,B] { self =>

    def inverse : IsoMorphismeAlgebrePSUG[B,A]

    class Lois(implicit ArbA : Arbitrary[A], ArbB : Arbitrary[B]) extends super.Lois {
      // Identique à morphisme2, en explicitant chaque operation
      def isoMorphisme = new RuleSet {
        val name    = "isoMorphisme AlgebrePSUG"
        val parents = Seq(morphisme1, morphisme2)
        val bases   = Seq("inverse morphisme 1" -> inverse.lois.morphisme1,
                          "inverse morphisme 1" -> inverse.lois.morphisme2
                         )
        val props   = Seq(
          "idA" -> forAll( (a:A) => inverse(self(a)) == a),
          "idB" -> forAll( (b:B) => self(inverse(b)) == b)
        )
      }
    }

    def lois(implicit ArbA : Arbitrary[A], ArbB : Arbitrary[B]) = new Lois
  }

  object IsoMorphismeAlgebrePSUG {
    def apply[A,B](f : A => B, g : B => A)(implicit A : AlgebrePSUG[A], B : AlgebrePSUG[B]) = new IsoMorphismeAlgebrePSUG[A,B] { self =>
      def apply(a: A): B = f(a)

      def inverse = new IsoMorphismeAlgebrePSUG[B,A] {
        def apply(b: B): A = g(b)
        def inverse: IsoMorphismeAlgebrePSUG[A, B] = self
      }
    }
  }

  // Structure Initiale par ADT

  sealed abstract class InitialPSUG_P
  case object ZeroP                                                extends InitialPSUG_P // Vide
  case class  PlusP(gauche : InitialPSUG_P, droit : InitialPSUG_P) extends InitialPSUG_P // Noeud

  object InitialPSUG_P {
    implicit final val algebrePSUG = new AlgebrePSUG[InitialPSUG_P] {
      def zero: InitialPSUG_P = ZeroP
      def plus(gauche: InitialPSUG_P, droit: InitialPSUG_P): InitialPSUG_P = PlusP(gauche, droit)
    }
  }

  def uniqueMorphisme_P[A](implicit A : AlgebrePSUG[A]) : MorphismeAlgebrePSUG[InitialPSUG_P, A] =
    MorphismeAlgebrePSUG[InitialPSUG_P,A]( (i: InitialPSUG_P) => i match {
      case ZeroP      => A.zero
      case PlusP(g,d) => A.plus( uniqueMorphisme_P[A].apply(g) , uniqueMorphisme_P[A].apply(d))
    })

  // Object initiale par point fixe

  final case class Fix[F[_]](self : F[Fix[F]]) // extends AnyVal

  type InitialPSUG_F = Fix[OperationPSUG]

  implicit final val initialPSUG_F = new AlgebrePSUG[InitialPSUG_F] {
    def zero: InitialPSUG_F = Fix(Zero : OperationPSUG[InitialPSUG_F])
    def plus(gauche: InitialPSUG_F, droit: InitialPSUG_F): InitialPSUG_F = Fix(Plus(gauche, droit) : OperationPSUG[InitialPSUG_F])
  }

  def uniqueMorphisme_F[A](implicit A : AlgebrePSUG[A]) : MorphismeAlgebrePSUG[InitialPSUG_F, A] =
    MorphismeAlgebrePSUG[InitialPSUG_F,A]( (i: InitialPSUG_F) => i match {
      case Fix(Zero)      => A.zero
      case Fix(Plus(g,d)) => A.plus( uniqueMorphisme_F[A].apply(g) , uniqueMorphisme_F[A].apply(d))
    })

  /**
    * Seulement une implémentation de fold est requise
    */
  trait InitialPSUG_I {
    def fold[A](implicit A : AlgebrePSUG[A]) : A = fold[A](A.zero)(A.plus)

    def fold[A](zero_ : A)(plus_ : (A,A) => A) : A = fold[A](new AlgebrePSUG[A] {
      def zero          : A = zero_
      def plus(g:A,d:A) : A = plus_(g,d)
    })

    override final def equals(o: Any): Boolean = o match {
      case l: InitialPSUG_I =>

        type X = InitialPSUG_I => Boolean

        val zero           : X = (ll: InitialPSUG_I) => ll.fold[Boolean](true)((_, _) => false)
        def plus(g:X, d:X) : X = (ll: InitialPSUG_I) => ll.fold[(Boolean, InitialPSUG_I)]((false, ZeroI))((x, y) => (g(x._2) && d(y._2), PlusI(x._2, y._2)))._1

        fold[X](zero)(plus)(l)
      case _ => false
    }

    def hasCode() : Int = toString.hashCode()

    override def toString = fold[String]("Z")((x,y)=> s"($x,$y)")
  }

  object InitialPSUG_I {
    implicit final val algebrePSUG = new AlgebrePSUG[InitialPSUG_I] {
      def zero: InitialPSUG_I = ZeroI
      def plus(gauche: InitialPSUG_I, droit: InitialPSUG_I): InitialPSUG_I = PlusI(gauche, droit)
    }

    implicit final val arbitrary : Arbitrary[InitialPSUG_I] = Arbitrary {
      def genPlus(n : Long) : Gen[InitialPSUG_I] =
        if (n <= 0) Gen.const(ZeroI)
        else for {
          g <- genADT(n-1)
          d <- genADT(n-1)
        } yield PlusI(g,d)

      def genADT(n : Long) : Gen[InitialPSUG_I] = Gen.oneOf(Gen.const(ZeroI : InitialPSUG_I),
                                                            genPlus(n)
                                                           )

      genADT(100)
    }
  }

  final val ZeroI = new InitialPSUG_I {
    override def fold[A](zero : A)(plus : (A,A) => A) : A = zero
  }

  final def PlusI(g:InitialPSUG_I, d:InitialPSUG_I) = new InitialPSUG_I {
    override def fold[A](zero : A)(plus : (A,A) => A) : A = plus(g.fold(zero)(plus), d.fold(zero)(plus))
  }

  def uniqueMorphisme_I[A : AlgebrePSUG] = MorphismeAlgebrePSUG[InitialPSUG_I,A]( (i: InitialPSUG_I) => i.fold[A])

  //
  // IsoMorphismes
  //

  val isoMorphisme_P_I = IsoMorphismeAlgebrePSUG(uniqueMorphisme_P[InitialPSUG_I].apply _, uniqueMorphisme_I[InitialPSUG_P].apply _)
  val isoMorphisme_I_F = IsoMorphismeAlgebrePSUG(uniqueMorphisme_I[InitialPSUG_F].apply _, uniqueMorphisme_F[InitialPSUG_I].apply _)
  val isoMorphisme_F_P = IsoMorphismeAlgebrePSUG(uniqueMorphisme_F[InitialPSUG_P].apply _, uniqueMorphisme_P[InitialPSUG_F].apply _)
}
