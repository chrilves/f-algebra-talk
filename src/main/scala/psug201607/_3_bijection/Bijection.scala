package psug201607._3_bijection

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline

trait <=>[A,B] {
  def from(a : A) : B
  def to(b : B)   : A

  def laws(implicit A : Arbitrary[A], B : Arbitrary[B]) = new discipline.Laws {
    def iso = new SimpleRuleSet("iso",
      "idA" -> forAll( (a:A) => to(from(a)) == a),
      "idB" -> forAll( (b:B) => from(to(b)) == b)
    )
  }
}