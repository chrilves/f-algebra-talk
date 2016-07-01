package psug201607

import org.scalacheck.{Arbitrary, Gen}
import psug201607._4_algebre.Algebre.AlgebrePSUG
import psug201607._5_morphisme.Morphisme.{FunctorCheck, IsoMorphismeAlgebre, MorphismeAlgebre}
import org.scalacheck.Shapeless._

object _7_liberte {

  type T = Char // Peut être n'importe quoi!

  // Operations

  sealed abstract class OperationLibrePSUG[+A]
  case object Zero                            extends OperationLibrePSUG[Nothing]
  case class  Plus[+A](gauche : A, droit : A) extends OperationLibrePSUG[A]
  case class  Elem(t  : T)                    extends OperationLibrePSUG[Nothing]

  object OperationLibrePSUG {
    implicit final val functorCheck = new FunctorCheck[OperationLibrePSUG] {
       def map[A, B](fa: OperationLibrePSUG[A])(f: (A) => B): OperationLibrePSUG[B] = fa match {
         case Zero      => Zero
         case Plus(g,d) => Plus(f(g),f(d))
         case Elem(t)   => Elem(t)
       }

      def arbitrary[A](implicit A: Arbitrary[A]): Arbitrary[OperationLibrePSUG[A]] = Arbitrary {
        def genPlus : Gen[OperationLibrePSUG[A]] =
          for {
            g <- A.arbitrary
            d <- A.arbitrary
          } yield Plus(g,d)

        def genOp : Gen[OperationLibrePSUG[A]] = Gen.oneOf(Gen.const(Zero),
                                                           implicitly[Arbitrary[Char]].arbitrary.map(Elem),
                                                           genPlus
                                                          )

        genOp
      }
    }
  }

  // Algebre

  trait AlgebreLibrePSUG[A] extends (OperationLibrePSUG[A] => A) {
    def zero           : A
    def plus(g:A, d:A) : A
    def elem(t : T)    : A

    final def apply(opa : OperationLibrePSUG[A]) : A = opa match {
      case Zero      => zero
      case Plus(g,d) => plus(g,d)
      case Elem(t)   => elem(t)
    }
  }

  object AlgebrePSUG {
    def apply[A](algebre : OperationLibrePSUG[A] => A) = new AlgebreLibrePSUG[A] {
      def zero                     : A = algebre(Zero)
      def plus(gauche:A, droite:A) : A = algebre(Plus(gauche, droite))
      def elem(t : T)              : A = algebre(Elem(t))
    }
  }



  //
  // Libre par ADT
  //


  abstract sealed class LibrePSUG_P
  case object ZeroP                                   extends LibrePSUG_P // Vide
  case class  PlusP(g : LibrePSUG_P, d : LibrePSUG_P) extends LibrePSUG_P // Noeud(gauche, droit)
  case class  ElemP(t : T)                            extends LibrePSUG_P // Feuille(t)

  object LibrePSUG_P {
    implicit final val algebrelibrePSUG = new AlgebreLibrePSUG[LibrePSUG_P] {
      def zero: LibrePSUG_P = ZeroP
      def plus(g: LibrePSUG_P, d: LibrePSUG_P): LibrePSUG_P = PlusP(g,d)
      def elem(t: T): LibrePSUG_P = ElemP(t)
    }
  }

  def uniqueMorphoismeP[A](implicit A : AlgebreLibrePSUG[A]) : MorphismeAlgebre[OperationLibrePSUG, LibrePSUG_P, A] =
    MorphismeAlgebre[OperationLibrePSUG, LibrePSUG_P, A]( (l : LibrePSUG_P) => l match {
      case ZeroP      => A.zero
      case PlusP(g,d) => A.plus(uniqueMorphoismeP[A].apply(g), uniqueMorphoismeP[A].apply(d))
      case ElemP(t)   => A.elem(t)
    })



  /**
    * Tous les fold sont équivalents
    */
  trait LibrePSUG_I {
    def fold[A](implicit A : AlgebreLibrePSUG[A]) : A = fold[A](A.zero)(A.plus)(A.elem)

    def fold[A](zero_ : A)(plus_ : (A,A) => A)(elem_ : T => A) : A = fold[A](new AlgebreLibrePSUG[A] {
      def zero            : A = zero_
      def plus(g: A, d: A): A = plus_(g,d)
      def elem(t: T)      : A = elem_(t)
    })

    def fold[A](elem : T => A)(implicit A : AlgebrePSUG[A]) : A = fold[A](A.zero)(A.plus)(elem)


    override final def equals(o: Any): Boolean = o match {
      case l: LibrePSUG_I =>

        type X = LibrePSUG_I => Boolean

        val zero           : X = (ll: LibrePSUG_I) => ll.fold[Boolean](true)((_, _) => false)(_ => false)
        def plus(g:X, d:X) : X = (ll: LibrePSUG_I) => ll.fold[(Boolean, LibrePSUG_I)]((false, ZeroI))((x, y) => (g(x._2) && d(y._2), PlusI(x._2, y._2)))(t => (false, ElemI(t)))._1
        def elem(t:T)      : X = (ll: LibrePSUG_I) => ll.fold[Boolean](false)((_, _) => false)(u => t == u)

        fold[X](zero)(plus)(elem)(l)
      case _ => false
    }

    def hasCode() : Int = toString.hashCode()

    override def toString = fold[String]("Zero")((x,y)=> s"Plus($x,$y)")(t => s"Elem($t)")
  }

  object LibrePSUG_I {
    implicit final val algebreLibrePSUG = new AlgebreLibrePSUG[LibrePSUG_I] {
      def zero: LibrePSUG_I = ZeroI
      def plus(g: LibrePSUG_I, d: LibrePSUG_I): LibrePSUG_I = PlusI(g,d)
      def elem(t: T): LibrePSUG_I = ElemI(t)
    }

    implicit final val arbitrary : Arbitrary[LibrePSUG_I] = Arbitrary {
      def genPlus(n : Long) : Gen[LibrePSUG_I] =
        if (n <= 0) Gen.const(ZeroI)
        else for {
          g <- genADT(n-1)
          d <- genADT(n-1)
        } yield PlusI(g,d)

      def genADT(n : Long) : Gen[LibrePSUG_I] = Gen.oneOf(Gen.const(ZeroI : LibrePSUG_I),
                                                          implicitly[Arbitrary[T]].arbitrary.map(ElemI),
                                                          genPlus(n)
                                                         )

      genADT(100)
    }
  }

  val ZeroI = new LibrePSUG_I {
    override def fold[A](zero_ : A)(plus_ : (A,A) => A)(elem_ : T => A) : A = zero_
  }

  def PlusI(g : LibrePSUG_I, d : LibrePSUG_I) = new LibrePSUG_I {
    override def fold[A](zero : A)(plus : (A,A) => A)(elem : T => A) : A = plus(g.fold[A](zero)(plus)(elem),d.fold[A](zero)(plus)(elem))
  }

  def ElemI(t : T) = new LibrePSUG_I {
    override def fold[A](zero : A)(plus : (A,A) => A)(elem : T => A) : A = elem(t)
  }


  def uniqueMorphoismeI[A : AlgebreLibrePSUG] : MorphismeAlgebre[OperationLibrePSUG, LibrePSUG_I, A] =
    MorphismeAlgebre[OperationLibrePSUG, LibrePSUG_I, A]( (l : LibrePSUG_I) => l.fold[A])


  //
  // Iso
  //

  val isoMorphismeAlgebre_P_I = IsoMorphismeAlgebre[OperationLibrePSUG,LibrePSUG_P, LibrePSUG_I](uniqueMorphoismeP[LibrePSUG_I], uniqueMorphoismeI[LibrePSUG_P])

  //
  // CoAlgèbre
  //

  type CoAlgebreLibrePSUG[A] = A => OperationLibrePSUG[A]

  trait CoLibrePSUG {
    def matcher: OperationLibrePSUG[CoLibrePSUG] // Vide ou Noeud ou Feuille
  }

}
