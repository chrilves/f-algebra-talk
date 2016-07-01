package psug201607.annexe.liste_monoid_non_iso

import org.typelevel.discipline
import org.scalacheck.{Arbitrary, Prop}
import Prop._

trait ListeAlg[I, A] {
  def vide : A
  def cons(tete : I , reste : A) : A
}

trait MonoidInj[I, A] extends ListeAlg[I, A] {
  def zero : A
  def plus(g : A, d : A) : A
  def inj(i : I) : A

  // Un Monoid sur I est une algèbre de liste sur I
  final def vide = zero
  final def cons(tete : I , reste : A) : A = plus(inj(tete) , reste)

  def laws(implicit arbA : Arbitrary[A]) = new discipline.Laws {
    def monoid = new SimpleRuleSet("monoid",
      "0 + a = a" -> forAll( (a:A) => plus(zero, a) == a),
      "a + 0 = a" -> forAll( (a:A) => plus(a, zero) == a),
      "(a+b)+c = a+(b+c)" -> forAll( (a:A, b:A, c:A) => plus(plus(a,b),c) == plus(a, plus(b,c)))
    )
  }
}

/** Egalité entre deux listes */
object EqListe {
  def apply[I : Arbitrary, A : Arbitrary](l1 : ListeAlg[I, A], l2 : ListeAlg[I, A]) = new discipline.Laws {
    def eqliste = new SimpleRuleSet("eq liste",
      "vide = vide" -> Prop( l1.vide == l2.vide ),
      "cons = cons" -> forAll( (tete : I, reste:A) => l1.cons(tete, reste) == l2.cons(tete, reste) )
    )
  }
}

sealed abstract class ListeOpt[+I]
final case object Vide                                            extends ListeOpt[Nothing]
final case class  Cons[+I](tete : Option[I], reste : ListeOpt[I]) extends ListeOpt[I]

abstract class ListeOptMonoid[I] extends MonoidInj[I, ListeOpt[I]] {
  final def zero = Vide
  final def inj(i : I) = Cons(Some(i), Vide)
}

class ListeOptConcat[I] extends ListeOptMonoid[I] {
  def plus(g: ListeOpt[I], d: ListeOpt[I]): ListeOpt[I] = g match {
    case Vide => d
    case Cons(x,l) => Cons(x, plus(l, d))
  }
}

class ListeOptAbsorb[I] extends ListeOptMonoid[I] {
  def plus(g: ListeOpt[I], d: ListeOpt[I]): ListeOpt[I] = d match {
    case Vide => g
    case _ => g match {
      case Vide => d
      case Cons( Some(i) , l) => Cons( Some(i), plus(l ,d ))
      case Cons( None    , _) => Cons(None, Vide)
    }
  }
}