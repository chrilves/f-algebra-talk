package psug201607.annexe.liste_monoid_non_iso

import org.scalatest.FunSuite
import org.scalacheck.{Prop, Test}
import org.scalacheck.Shapeless._
import org.scalacheck.Prop._

class ListeMonoidNonIso extends FunSuite with org.typelevel.discipline.scalatest.Discipline {

  val lconcat = new ListeOptConcat[Int]
  val labsorb = new ListeOptAbsorb[Int]

  checkAll("ListeCCMonoid1" , lconcat.laws.monoid)
  checkAll("ListeCCMonoid2" , labsorb.laws.monoid)
  checkAll("Eq Liste"       , EqListe(lconcat, labsorb).eqliste)

  test("Contre exemple") {
    check(Prop {
      val gauche = Cons(None   , Vide)
      val droit  = Cons(Some(0), Vide)

      lconcat.plus(gauche, droit) != labsorb.plus(gauche, droit)
    })
  }
}
