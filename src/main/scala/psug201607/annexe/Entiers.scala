package psug201607.annexe

import org.scalacheck.Arbitrary
import org.typelevel.discipline
import org.scalacheck.Prop._

object Entiers {


  trait AlgebreInt[A] {
    def zero   : A
    def un     : A

    def plus(x : A, y : A) : A
    def oppose(x  : A) : A

    def mult(x : A, y : A) : A

    class Laws(implicit ArbA : Arbitrary[A]) extends discipline.Laws {
      def algebre = new SimpleRuleSet("algebre",
        "plus assoc"  -> forAll( (a:A, b:A, c:A) => plus(a, plus(b,c)) == plus(plus(a,b), c)),
        "plus commut" -> forAll( (a:A, b:A)      => plus(a, b) == plus(b , a)),
        "plus zero"   -> forAll( (a:A)           => plus(zero, a) == a),

        "mult assoc"  -> forAll( (a:A, b:A, c:A) => mult(a, mult(b,c)) == mult(mult(a,b), c)),
        "mult commut" -> forAll( (a:A, b:A)      => mult(a, b) == mult(b , a)),
        "mult un"     -> forAll( (a:A)           => mult(un, a) == a),

        "mult zero"   -> forAll( (a:A)           => mult(zero, a) == zero),
        "distrib"     -> forAll( (a:A,b:A,c:A)   => mult(a, plus(b,c)) == plus(mult(a , b), mult(a,c))),

        "opp"         -> forAll( (a:A)           => plus(oppose(a), a) == zero),
        "opp . opp"   -> forAll( (a:A)           => oppose(oppose(a)) == a)
      )
    }

    def laws(implicit ArbA : Arbitrary[A]) = new Laws
  }

  object Erreur {

    sealed abstract class Algebre
    case object Zero                            extends Algebre
    case object Un                              extends Algebre
    case class  Plus(a : Algebre, b : Algebre)  extends Algebre
    case class  Oppose(a : Algebre)             extends Algebre
    case class  Mult(a : Algebre, b:Algebre)    extends Algebre

    assert((Plus(Zero, Zero) : Algebre) != (Zero : Algebre))
  }



  sealed abstract class Entier
  sealed abstract class EntierSur         extends Entier // ]-inf, -2] + [1, inf[

  final case object Zero                  extends Entier
  final case object MoinsUn               extends Entier

  final case object Un                    extends EntierSur
  final case object MoinsDeux             extends EntierSur
  final case class  Pair(n : EntierSur)   extends EntierSur
  final case class  Impair(n : EntierSur) extends EntierSur


  final class AlgebreIntOps[A](val self : A)(implicit A : AlgebreInt[A]) {
    def |+|(a : A) : A = A.plus(self, a)
    def |*|(a : A) : A = A.mult(self, a)
    def opp : A = A.oppose(self)
  }


  object Entier {
    def toLong(n : Entier) : Long = n match {
      case Zero      => 0L
      case Un        => 1L
      case MoinsDeux => -2L
      case MoinsUn   => -1L
      case Pair(n)   => toLong(n) * 2L
      case Impair(n) => toLong(n) * 2L + 1
    }
  }

  object AlgebreIntEntier extends AlgebreInt[Entier] {

    def double(n : Entier) : Entier = n match {
      case Zero          => Zero
      case MoinsUn       => MoinsDeux
      case n : EntierSur => Pair(n)
    }

    def doublePlus1(n : Entier) : Entier = n match {
      case Zero          => Un
      case MoinsUn       => MoinsUn
      case n : EntierSur => Impair(n)
    }

    def succ(n : Entier) : Entier = n match {
      case Zero      => Un
      case MoinsUn   => Zero
      case MoinsDeux => MoinsUn
      case Un        => Pair(Un)
      case Pair(n)   => Impair(n)
      case Impair(n) => double(succ(n))
    }

    def pred(n : Entier) : Entier = n match {
      case Zero      => MoinsUn
      case MoinsUn   => MoinsDeux
      case MoinsDeux => Impair(MoinsDeux)
      case Un        => Zero
      case Pair(n)   => doublePlus1(pred(n))
      case Impair(n) => Pair(n)
    }

    def oppose(n : Entier) : Entier = n match {
      case Zero      => Zero
      case MoinsUn   => Un
      case MoinsDeux => Pair(Un)
      case Un        => MoinsUn
      case Pair(n)   => double(oppose(n))
      case Impair(n) => pred(double(oppose(n)))
    }

    final def moins(x : Entier, y : Entier) : Entier = plus(x , oppose(y))

    def zero: Entier = Zero
    def un  : Entier = Un

    def plus(x: Entier, y: Entier): Entier = (x,y) match {
      case (Zero     ,  _        ) => y
      case (_        , Zero      ) => x
      case (Un       , _         ) => succ(y)
      case (_        , Un        ) => succ(x)
      case (MoinsUn  , _         ) => pred(y)
      case (_        , MoinsUn   ) => pred(x)
      case (MoinsDeux, _         ) => pred(pred(y))
      case ( _       , MoinsDeux ) => pred(pred(x))
      case (Pair(n)  , Pair(m)   ) => double(plus(n,m))
      case (Pair(n)  , Impair(m) ) => doublePlus1(plus(n,m))
      case (Impair(n), Pair(m)   ) => doublePlus1(plus(n,m))
      case (Impair(n), Impair(m) ) => double(succ(plus(n,m)))
    }

    def mult(x: Entier, y: Entier): Entier = (x,y) match {
      case (Zero     ,  _        ) => Zero
      case (_        , Zero      ) => Zero
      case (Un       , _         ) => y
      case (_        , Un        ) => x
      case (MoinsUn  , _         ) => oppose(y)
      case (_        , MoinsUn   ) => oppose(x)
      case (MoinsDeux, _         ) => oppose(double(y))
      case ( _       , MoinsDeux ) => oppose(double(x))
      case (Pair(n)  , Pair(m)   ) => double(double(mult(n,m)))
      case (Pair(n)  , Impair(m) ) => double(plus(double(mult(n,m)), n))
      case (Impair(n), Pair(m)   ) => double(plus(double(mult(n,m)), m))
      case (Impair(n), Impair(m) ) => plus(double(double(mult(n,m))) , doublePlus1(plus(n,m)))
    }
  }
}
