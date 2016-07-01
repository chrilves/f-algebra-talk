package psug201607

import scala.util.hashing._

object _2_pitch {

  // Predicatif

  sealed abstract class ListeP[+A]
  case object VideP                                  extends ListeP[Nothing]
  case class  ConsP[+A](tete : A, reste : ListeP[A]) extends ListeP[A]

  // Impredicatif

  trait ListeI[+A] {
    def fold[R](vide : R)(cons : (A, R) => R) : R

    override final def equals(o: Any): Boolean = o match {
      case l: ListeI[Any] =>

        type X = ListeI[Any] => Boolean

        val nil                  : X = (ll: ListeI[Any]) => ll.fold[Boolean](true)((_, _) => false)
        def cons(hd: Any, tl: X) : X = (ll: ListeI[Any]) => ll.fold[(Boolean, ListeI[Any])]((false, NilI[Any]))((a, f) => ((hd == a) && tl(f._2), ConsI[Any](a, f._2)))._1

        fold[X](nil)(cons)(l)
      case _ => false
    }

    def hasCode() : Int = MurmurHash3.finalizeHash(fold[Int](0)((a, i) => MurmurHash3.mix(i , a.hashCode()))
                                                  ,fold[Int](0)((a, i) => i + 4))
  }

  def NilI[A] = new ListeI[A] {
    def fold[R](vide : R)(cons : (A, R) => R) : R = vide
  }

  def ConsI[A](tete : A, reste : ListeI[A]) = new ListeI[A] {
    def fold[R](vide : R)(cons : (A, R) => R) : R = cons(tete, reste.fold(vide)(cons))
  }

  // Stream

  type StreamP[+A] = scala.collection.immutable.Stream[A]

  trait StreamI[+A] {
    def matcher[R](vide : R)(cons : (A, StreamI[A]) => R) : R
  }
}