package psug201607

object _1_intro {

  // Case class

  case class Voiture(model : String, portes : Int, couleur : String)

  val voiture : Voiture = Voiture("206", 3, "Blanche")

  def penture(v : Voiture, c : String) : Voiture = v match {
    case Voiture(model, portes, couleur) => Voiture(model, portes, c)
  }

  // Famille Scellées

  sealed abstract class Couleur
  case object Carreaux extends Couleur
  case object Coeur    extends Couleur
  case object Pique    extends Couleur
  case object Trefle   extends Couleur

  def rouge(c : Couleur) : Boolean = c match {
    case Carreaux => true
    case Coeur    => true
    case Pique    => false
    case Trefle   => false
  }

  // Listes

  sealed abstract class Liste[+A]
  final case object Vide                                 extends Liste[Nothing]
  final case class  Cons[+A](tete : A, reste : Liste[A]) extends Liste[A]
  val vide         = Vide
  val singleton    = Cons(1, Vide)
  val deuxValeurs  = Cons(3, singleton)
  val deuxValeurs2 = Cons(3, Cons(1, Vide))

  assert(deuxValeurs == deuxValeurs2)

  def taille[A](l : Liste[A]) : Int = l match {
    case Vide           => 0
    case Cons(_, reste) => 1 + taille(reste)
  }
}