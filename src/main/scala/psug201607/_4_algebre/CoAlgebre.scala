package psug201607._4_algebre

object CoAlgebre {

  // CoAlgebre Liste

  abstract sealed class OperationListe[+I, +A]
  case object Vide                             extends OperationListe[Nothing, Nothing]
  case class  Cons[I , A](tete : I, reste : A) extends OperationListe[I,A]

  type CoAlgebreListe[I, A] = A => OperationListe[I, A]

  // Entier => Liste booleens

  def decompose : CoAlgebreListe[Boolean, Long] =
    (a : Long) => if (a <= 0) Vide
                  else Cons(a % 2 == 1, a / 2)

  // 5 = 101
  assert(decompose(5) == Cons(true , 2))
  assert(decompose(2) == Cons(false, 1))
  assert(decompose(1) == Cons(true , 0))
  assert(decompose(0) == Vide)

  def puissance(n : Long, p : Long) : Long = decompose(p) match {
    case Vide           => 1
    case Cons(false, r) => puissance(n * n, r)
    case Cons(true , r) => puissance(n * n, r) * r
  }

  // Labyrynthe

  sealed abstract  class Cas[+A]
  case object Sortie                                        extends Cas[Nothing]
  case object CulDeSac                                      extends Cas[Nothing]
  case class  Bifurcation[+A](chemin0 : (String, A),
                              chemin1 : (String, A),
                              chemins : List[(String , A)]) extends Cas[A]


  trait Labyrinthe[A] extends (A => Cas[A]) {
    def apply(a : A) : Cas[A]
  }

  def parcour[A](a : A)(implicit A : Labyrinthe[A]) : Unit = A(a) match {
    case Sortie                   => println("Gagné!")
    case CulDeSac                 => println("Perdu!")
    case Bifurcation(un, deux, l) =>
      val chemins = un :: deux :: l
      parcour(chemins(scala.io.StdIn.readInt())._2)
  }
}
