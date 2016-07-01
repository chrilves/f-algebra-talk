package psug201607._8_lois

import org.typelevel.discipline
import org.scalacheck.{Arbitrary, Gen, Prop}
import psug201607._4_algebre.Algebre.AlgebrePSUG
import Prop._
import psug201607._4_algebre.CoAlgebre.{Cons, OperationListe, Vide}
import psug201607._5_morphisme.Morphisme.{Algebre, CoAlgebre, IsoMorphismeAlgebre, MorphismeAlgebre}
import psug201607._7_liberte
import psug201607._7_liberte.{AlgebreLibrePSUG, LibrePSUG_P, OperationLibrePSUG, T}

import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3

object Algebre {

  //
  // Lois
  //

  trait Monoid[A] extends AlgebrePSUG[A]{
    def zero : A
    def plus(g:A, d:A) : A

    def lois(implicit ArbA : Arbitrary[A]) = new discipline.Laws {
      def monoid = new SimpleRuleSet("monoid",
        "0 + a = a"         -> forAll( (a:A) => plus(zero, a) == a),
        "a + 0 = a"         -> forAll( (a:A) => plus(a, zero) == a),
        "(a+b)+c = a+(b+c)" -> forAll( (a:A,b:A,c:A) => plus(plus(a, b), c) == plus(a, plus(b,c)))
      )
    }
  }


  //
  // Normalisation
  //

  def simplifier(x : LibrePSUG_P) : Option[LibrePSUG_P] = x match {
    case _7_liberte.PlusP(_7_liberte.ZeroP, a)      => Some(a)
    case _7_liberte.PlusP(a, _7_liberte.ZeroP)      => Some(a)
    case _7_liberte.PlusP(_7_liberte.PlusP(a,b), c) => Some(_7_liberte.PlusP(a, _7_liberte.PlusP(b,c)))
    case _                                          => None
  }

  @tailrec
  def simplifierMax(x : LibrePSUG_P) : LibrePSUG_P = simplifier(x) match {
    case Some(y) => simplifierMax(y)
    case None    => x
  }

  def normaliser(x : LibrePSUG_P) : LibrePSUG_P = simplifierMax(x) match {
    case _7_liberte.PlusP(a, b) => _7_liberte.PlusP(normaliser(a), normaliser(b))
    case s                      => s
  }

  //
  // Résultat
  //

  sealed abstract class MonoidLibreP
  object MonoidLibreP {
    case object ZeroP                                     extends MonoidLibreP
    case class  PlusElemP(tete : T, reste : MonoidLibreP) extends MonoidLibreP

    implicit final val monoid = new Monoid[MonoidLibreP] {
      def zero: MonoidLibreP = ZeroP
      def plus(g: MonoidLibreP, d: MonoidLibreP): MonoidLibreP = g match {
        case ZeroP => d
        case PlusElemP(t, x) => PlusElemP(t, plus(x, d))
      }
    }

    implicit final val algebreLibrePSUG = new AlgebreLibrePSUG[MonoidLibreP] {
      def zero: MonoidLibreP = ZeroP

      def plus(g: MonoidLibreP, d: MonoidLibreP): MonoidLibreP = g match {
        case ZeroP => d
        case PlusElemP(t, x) => PlusElemP(t, plus(x, d))
      }

      def elem(t: T): MonoidLibreP = PlusElemP(t, ZeroP)
    }
  }

  type OperationListeT[+A] = OperationListe[T, A]
  type AlgebreListeT[A]    = Algebre[OperationListeT, A]
  type CoAlgebreListeT[A]  = CoAlgebre[OperationListeT, A]

  def uniqueMorphismeP[A](elem : T => A)(implicit A : Monoid[A]) : MorphismeAlgebre[OperationLibrePSUG, MonoidLibreP, A] = {
    implicit val algebreLibrePSUGA = new AlgebreLibrePSUG[A] {
      def zero: A = A.zero
      def plus(g: A, d: A): A = A.plus(g,d)
      def elem(t: T): A = elem(t)
    }

    MorphismeAlgebre[OperationLibrePSUG, MonoidLibreP, A]( (i : MonoidLibreP) => i match {
      case MonoidLibreP.ZeroP          => A.zero
      case MonoidLibreP.PlusElemP(t,j) => A.plus(elem(t), uniqueMorphismeP[A](elem).apply(j))
    })
  }



  trait MonoidLibreI {
    def foldAlgebreListeT[A](implicit A : AlgebreListeT[A]) : A =
      foldAlgebreListeT[A](A(Vide))((x,y)=> A(Cons(x,y)))

    def foldAlgebreListeT[A](zero : A)(plusElem : (T,A) => A) : A =
      foldAlgebreListeT[A] { (opa : OperationListeT[A]) => opa match {
        case Vide      => zero
        case Cons(t,a) => plusElem(t,a)
      }}

    def foldMonoid[A](elem : T => A)(implicit A : Monoid[A]) : A = foldAlgebreListeT[A](A.zero)((t,a) => A.plus(elem(t), a))


    override final def equals(o: Any): Boolean = o match {
      case l: MonoidLibreI =>

        type X = MonoidLibreI => Boolean

        val zero               : X = (ll: MonoidLibreI) => ll.foldAlgebreListeT[Boolean](true)((_, _) => false)
        def plusElem(t:T, x:X) : X = (ll: MonoidLibreI) => ll.foldAlgebreListeT[(Boolean, MonoidLibreI)]((false, ZeroI))((u, y) => ((t == u) && x(y._2) , PlusElemI(u, y._2)))._1

        foldAlgebreListeT[X](zero)(plusElem)(l)
      case _ => false
    }

    def hasCode() : Int = MurmurHash3.finalizeHash(foldAlgebreListeT(ZeroI.hasCode())((t,x) => MurmurHash3.mix(x, t.hashCode()))
                                                  ,foldAlgebreListeT(0)((t,x) => x + 1))

    override def toString = foldAlgebreListeT[String]("Zero")((x,y)=> s"Plus(Elem($x),$y)")
  }

  object MonoidLibreI {
    implicit final val monoid = new Monoid[MonoidLibreI] {
      def zero: MonoidLibreI = ZeroI
      def plus(g: MonoidLibreI, d: MonoidLibreI): MonoidLibreI = PlusI(g,d)
    }

    implicit final val algebreLibrePSUG = new AlgebreLibrePSUG[MonoidLibreI] {
      def zero: MonoidLibreI = ZeroI
      def plus(g: MonoidLibreI, d: MonoidLibreI): MonoidLibreI = PlusI(g,d)
      def elem(t: T): MonoidLibreI = ElemI(t)
    }

    implicit final val arbitrary : Arbitrary[MonoidLibreI] = Arbitrary {
      def genPlus : Gen[MonoidLibreI] =
        for {
          t <- implicitly[Arbitrary[T]].arbitrary
          d <- genADT
        } yield PlusElemI(t,d)

      def genADT : Gen[MonoidLibreI] = Gen.oneOf(Gen.const(ZeroI), genPlus)

      genADT
    }
  }

  val ZeroI = new MonoidLibreI {
    override def foldAlgebreListeT[A](zero : A)(plusElem : (T,A) => A) : A = zero
  }

  def PlusElemI(t : T, x : MonoidLibreI) = new MonoidLibreI {
    override def foldAlgebreListeT[A](zero : A)(plusElem : (T,A) => A) : A = plusElem(t, x.foldAlgebreListeT[A](zero)(plusElem))
  }

  def ElemI(t : T) : MonoidLibreI = PlusElemI(t, ZeroI)


  def PlusI(g:MonoidLibreI, d:MonoidLibreI) = new MonoidLibreI {
    override def foldAlgebreListeT[A](zero : A)(plusElem : (T,A) => A) : A =
      g.foldAlgebreListeT[MonoidLibreI](d)((t,m) => PlusElemI(t,m)).foldAlgebreListeT[A](zero)(plusElem)
  }

  def uniqueMorphismeI[A](elem_ : T => A)(implicit A : Monoid[A]) : MorphismeAlgebre[OperationLibrePSUG, MonoidLibreI, A] = {
    implicit val algebreLibrePSUGA = new AlgebreLibrePSUG[A] {
      def zero: A = A.zero
      def plus(g: A, d: A): A = A.plus(g,d)
      def elem(t: T): A = elem(t)
    }

    MorphismeAlgebre[OperationLibrePSUG, MonoidLibreI, A]( (i : MonoidLibreI) =>
      i.foldAlgebreListeT(A.zero)((t,x) => A.plus(elem_(t), x))
    )
  }



  //
  // IsoMorphismes
  //

  val isoMorphismeMonoid_I_P = IsoMorphismeAlgebre[OperationLibrePSUG, MonoidLibreP, MonoidLibreI](uniqueMorphismeP[MonoidLibreI](ElemI), uniqueMorphismeI[MonoidLibreP](MonoidLibreP.algebreLibrePSUG.elem))
}
