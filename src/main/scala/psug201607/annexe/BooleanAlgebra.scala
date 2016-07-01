package psug201607.annexe

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.typelevel.discipline
import psug201607.annexe.BooleanAlgebraOps._

trait BooleanAlgebra[A] {
  val ⊤ : A
  val ⊥ : A

  def ∧(l: A, r: =>A): A
  def ∨(l: A, r: =>A): A

  def ¬(l: A): A

  implicit final val self : this.type = this

  class Laws(implicit A : Arbitrary[A]) extends discipline.Laws {
    final def boolean = new SimpleRuleSet("boolean algebra",
      "identity       ∧" -> forAll( (a:A)         => (a ∧ ⊤) == a),
      "complements    ∧" -> forAll( (a:A)         => (a ∧ ¬(a)) == ⊥),
      "commutativity  ∧" -> forAll( (a:A,b:A)     => (a ∧ b) == (b ∧ a)),
      "associativity  ∧" -> forAll( (a:A,b:A,c:A) => ((a ∧ b) ∧ c) == (a ∧ (b ∧ c)) ),
      "distributivity ∧" -> forAll( (a:A,b:A,c:A) => (a ∧ (b ∨ c)) == ((a ∧ b) ∨ (a ∧ c)) ),

      "identity       ∨" -> forAll( (a:A)         => (a ∨ ⊥) == a),
      "complements    ∨" -> forAll( (a:A)         => (a ∨ ¬(a)) == ⊤),
      "commutativity  ∨" -> forAll( (a:A,b:A)     => (a ∨ b) == (b ∨ a)),
      "associativity  ∨" -> forAll( (a:A,b:A,c:A) => ((a ∨ b) ∨ c) == (a ∨ (b ∨ c)) ),
      "distributivity ∧" -> forAll( (a:A,b:A,c:A) => (a ∨ (b ∧ c)) == ((a ∨ b) ∧ (a ∨ c)) )
    )
  }

  def laws(implicit A : Arbitrary[A]) = new Laws
}

object BooleanAlgebra {
  def apply[A](implicit A : BooleanAlgebra[A]) : BooleanAlgebra[A] = A
}

sealed abstract class BooleanFunctor[+A]
case object True                 extends BooleanFunctor[Nothing]
case object False                extends BooleanFunctor[Nothing]
case class  Not[+A](a : A)       extends BooleanFunctor[A]
case class  And[+A](l : A, r: A) extends BooleanFunctor[A]


final class BooleanAlgebraOps[A](self : A)(implicit A : BooleanAlgebra[A]) {
  @inline def ∧(r: =>A): A = A.∧(self, r)
  @inline def ∨(r: =>A): A = A.∨(self, r)
  @inline def ¬ : A = A.¬(self)
}

object BooleanAlgebraOps {
  @inline implicit final def toBooleanAlgebraOps[A : BooleanAlgebra](self : A) : BooleanAlgebraOps[A] = new BooleanAlgebraOps[A](self)
}


object BooleanAlgebraInstances {

  implicit final val booleanBool = new BooleanAlgebra[Boolean] {
    val ⊥ : Boolean = false
    val ⊤ : Boolean = true

    def ¬(l: Boolean): Boolean = !l

    def ∨(l: Boolean, r: => Boolean): Boolean = l || r
    def ∧(l: Boolean, r: => Boolean): Boolean = l && r
  }


  sealed abstract class Couleur extends Product with Serializable
  case object Carreaux extends Couleur
  case object Treffle  extends Couleur
  case object Coeur    extends Couleur
  case object Piques   extends Couleur

  object Couleur {
    final val all : Set[Couleur] = Set(Carreaux, Treffle, Coeur, Piques)
  }

  implicit final val setCouleur = new BooleanAlgebra[Set[Couleur]] {
    val ⊥ : Set[Couleur] = Set.empty
    val ⊤ : Set[Couleur] = Couleur.all

    def ¬(l: Set[Couleur]): Set[Couleur] = ⊤ -- l

    def ∨(l: Set[Couleur], r: => Set[Couleur]): Set[Couleur] = l.union(r)
    def ∧(l: Set[Couleur], r: => Set[Couleur]): Set[Couleur] = l.intersect(r)
  }

  implicit final val seqCouleur = new BooleanAlgebra[Seq[Couleur]] {
    val ⊥ : Seq[Couleur] = Seq.empty
    val ⊤ : Seq[Couleur] = Couleur.all.toSeq

    def ¬(l: Seq[Couleur]): Seq[Couleur] = ⊤.filterNot(l.contains)

    def ∨(l: Seq[Couleur], r: => Seq[Couleur]): Seq[Couleur] = l.union(r)
    def ∧(l: Seq[Couleur], r: => Seq[Couleur]): Seq[Couleur] = l.intersect(r)
  }
}