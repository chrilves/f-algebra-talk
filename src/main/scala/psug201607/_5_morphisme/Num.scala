package psug201607._5_morphisme

import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline
import org.scalacheck.Shapeless._

/**
  * Created by christophe on 27/07/16.
  */
object Num {

  // Algebre

  trait Num[A] extends (Op[A] => A) {
    def zero : A
    def un   : A
    def plus(x : A, y : A) : A
    def mult(x : A, y : A) : A

    final def apply(opa : Op[A]) : A = opa match {
      case Zero      => zero
      case Un        => un
      case Plus(g,d) => plus(g,d)
      case Mult(g,d) => mult(g,d)
    }

    private def iter(f : A => A)(k : Long)(a : A) : A =
      if (k == 0) a
      else iter(f)(k-1)(f(a))

    // calcule: k * a
    final def scalaire(k : Long, a : A) : A = {
      assert(k >= 0)
      iter(plus(a,_))(k)(zero)
    }

    // calcule a^k
    final def puissance(k : Long, a : A) : A = {
      assert(k >= 0)
      iter(mult(a,_))(k)(un)
    }
  }

  object Num {
    def apply[A](algebre : Op[A] => A) = new Num[A] {
      def zero               : A = algebre(Zero      : Op[A])
      def un                 : A = algebre(Un        : Op[A])
      def plus(x : A, y : A) : A = algebre(Plus(x,y) : Op[A])
      def mult(x : A, y : A) : A = algebre(Mult(x,y) : Op[A])
    }
  }

  //
  // Operations
  //

  sealed abstract class Op[+A] {
    def map[B](f : A => B) : Op[B] = this match {
      case Zero => Zero
      case Un   => Un
      case Plus(x,y) => Plus(f(x), f(y))
      case Mult(x,y) => Mult(f(x), f(y))
    }
  }

  case object Zero               extends Op[Nothing]
  case object Un                 extends Op[Nothing]
  case class  Plus[+A](g:A, d:A) extends Op[A]
  case class  Mult[+A](g:A, d:A) extends Op[A]

  //
  // Long
  //

  implicit object LongNum extends Num[Long] {
    def zero = 0
    def un   = 1
    def plus(gauche: Long, droite: Long): Long = gauche + droite
    def mult(gauche: Long, droite: Long): Long = gauche * droite
  }


  //
  // List
  //

  implicit object ListNum extends Num[List[Any]] {
    def zero = Nil
    def un   = List("")
    def plus(gauche: List[Any], droite: List[Any]): List[Any] = gauche ++ droite
    def mult(gauche: List[Any], droite: List[Any]): List[Any] =
      for {
        g <- gauche
        d <- droite
      } yield (g,d)
  }

  assert(ListNum.plus(List(1,2,3), List('a','b')) == List(1,2,3,'a','b'))

  assert(ListNum.mult(List(1,2,3), List('a','b')) == List( (1,'a'), (1,'b'),
                                                           (2,'a'), (2,'b'),
                                                           (3,'a'), (3,'b')
                                                         )
        )

  //
  // Polynomes
  //

  /**
    * Represente le polynome kX^p
    *
    * Exemple: kXp(7,2) = 7X²
    */

  final case class kXp(facteur : Long, exposant : Long)
  type Polynome = List[kXp]

  // Evalue le polynome `p` en la valeur `a` (avec l'agèbre `A`)
  def evalPolynomeEn[A](p : Polynome)(a : A)(implicit A : Num[A]) : A =
    p.foldRight(A.zero) { case (kXp, res) => A.plus(res, A.scalaire(kXp.facteur, A.puissance(kXp.exposant, a))) }



  val polynomeTest = List(kXp(4L,9L), kXp(8L,3L))

  val valeurLong = evalPolynomeEn(polynomeTest)(11L)
  //val valeurList = evalPolynomeEn(polynomeTest)(List(0,1,2,3,4,5,6,7,8,9,10))

  def benchmark(poly : Polynome)(l : List[Any]) : Unit = {

    val n = l.length.toLong

    val time1 = System.currentTimeMillis()
    val resLong = evalPolynomeEn(poly)(n)
    val time2  = System.currentTimeMillis()

    println(s"[Long] $resLong in ${time2 - time1} millis")

    val time3  = System.currentTimeMillis()
    val resList = evalPolynomeEn(poly)(l)
    val time4  = System.currentTimeMillis()


    println(s"[List] ${resList.length} in ${time4 - time3} millis")
  }


  //
  // Corespondance
  //

  def taille : MorphismeNum[List[Any], Long] = MorphismeNum((l : List[Any]) => l.length.toLong)

  assert(taille(ListNum.zero        ) == LongNum.zero)
  assert(taille(ListNum.un          ) == LongNum.un)
  assert(taille(ListNum.plus(List(1,2,3) , List(4,5))) == LongNum.plus(taille(List(1,2,3)) , taille(List(4,5))))
  assert(taille(ListNum.mult(List(1,2,3) , List(4,5))) == LongNum.mult(taille(List(1,2,3)) , taille(List(4,5))))


  //
  // Morphisme Num
  //

  abstract class MorphismeNum[A,B](implicit A : Num[A], B : Num[B]) extends (A => B) { self =>

    class Lois(implicit ArbA : Arbitrary[A]) extends discipline.Laws {
      // Identique à morphisme2, en explicitant chaque operation
      def morphisme1 = new SimpleRuleSet("morphisme d'algebre Num",
        "zero" -> Prop(self(A.zero) == B.zero),
        "un"   -> Prop(self(A.un)   == B.un),
        "plus" -> forAll((x:A,y:A) => self(A.plus(x,y)) == B.plus(self(x), self(y))),
        "mult" -> forAll((x:A,y:A) => self(A.mult(x,y)) == B.mult(self(x), self(y)))
      )

      // Identique a morphisme1
      def morphisme2 = new SimpleRuleSet("morphisme d'algebre Num",
        "morphisme" -> forAll((opa : Op[A]) => self(A(opa)) == B(opa.map(self)))
      )
    }

    def lois(implicit ArbA : Arbitrary[A]) = new Lois
  }

  object MorphismeNum {
    def apply[A,B](f : A => B)(implicit A : Num[A], B : Num[B]) = new MorphismeNum[A,B] {
      def apply(a: A): B = f(a)
    }
  }




  //
  // IsoMorphisme Num
  //


  abstract class IsoMorphismeNum[A,B](implicit A : Num[A], B : Num[B]) extends MorphismeNum[A,B] { self =>

    def inverse : IsoMorphismeNum[B,A]

    class Lois(implicit ArbA : Arbitrary[A], ArbB : Arbitrary[B]) extends super.Lois {
      // Identique à morphisme2, en explicitant chaque operation
      def isoMorphismeNum = new RuleSet {
        val name    = "isoMorphismeNum"
        val parents = Seq(morphisme1, morphisme2)
        val bases   = Seq("inverse morphisme 1" -> inverse.lois.morphisme1,
                          "inverse morphisme 2" -> inverse.lois.morphisme2
                         )
        val props   = Seq(
          "idA" -> forAll( (a:A) => inverse(self(a)) == a),
          "idB" -> forAll( (b:B) => self(inverse(b)) == b)
        )
      }
    }

    def lois(implicit ArbA : Arbitrary[A], ArbB : Arbitrary[B]) = new Lois
  }

  object IsoMorphismeNum {
    def apply[A,B](f : A => B, g : B => A)(implicit A : Num[A], B : Num[B]) = new IsoMorphismeNum[A,B] { self =>
      def apply(a: A): B = f(a)

      def inverse = new IsoMorphismeNum[B,A] {
        def apply(b: B): A = g(b)
        def inverse: IsoMorphismeNum[A, B] = self
      }
    }
  }



}
