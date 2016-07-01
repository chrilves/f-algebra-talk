package psug201607.annexe

import scala.annotation.tailrec

abstract class Inf[A] { self =>
  def head : A
  def tail : Inf[A]

  final def zip[B](cl : Inf[B]) : Inf[(A,B)] = new Inf[(A,B)] {
    def head = (self.head, cl.head)
    def tail : Inf[(A,B)] = self.tail.zip(cl.tail)
  }

  final def ap[B](cl : Inf[A => B]) : Inf[B] = new Inf[B] {
    def head = cl.head(self.head)
    def tail = self.tail.ap(cl.tail)
  }

  final def map[B](f : A => B) : Inf[B] = new Inf[B] {
    def head = f(self.head)
    def tail = self.tail.map(f)
  }

  @tailrec
  final def drop(n : Int) : Inf[A] =
    if (n <= 0) self
    else self.tail.drop(n-1)

  @tailrec
  final def dropWhile(p : A => Boolean) : Inf[A] = {
    val x = self.head
    if (p(x)) self.tail.dropWhile(p)
    else Inf(x)(self.tail)
  }

  final def take(n : Int) : List[A] =
    if (n == 0) List.empty[A]
    else self.head :: self.tail.take(n - 1)

  final def takeWhile(p : A => Boolean): List[A] = {
    val x = self.head

    if (p(x)) x :: self.takeWhile(p)
    else List.empty
  }

  @tailrec
  final def foreach(f : A => Unit) : Unit = {
    f(self.head)
    self.tail.foreach(f)
  }

  @tailrec
  final def for_(n : Long)(f : A => Unit) : Unit =
    if (n > 0) {
      f(self.head)
      self.tail.for_(n-1)(f)
    }

  final def filter(p : A => Boolean) : Inf[A] = {
    val x = self.head

    if (p(x)) Inf(x)(self.tail.filter(p))
    else self.tail.filter(p)
  }

  final def flatMap[B](f : A => Inf[B]) : Inf[B] = f(self.head)

  def ++(o : =>Inf[A]) : Inf[A] = self


  def interleave(o : Inf[A]) : Inf[A] = new Inf[A] {
    def head = self.head
    def tail = Inf(o.head)(self.tail.interleave(o.tail))
  }
}

object Inf {
  def apply[A](head_ : => A)(tail_ : => Inf[A]) : Inf[A] = new Inf[A] {
    def head = head_
    def tail = tail_
  }

  def laz[A](cl : => Inf[A]) : Inf[A] = new Inf[A] {
    final lazy val x = cl

    def head = x.head
    def tail = x.tail
  }

  def merge[A](l : List[Inf[A]]) : Inf[A] = {
    val init: Inf[A] = laz(merge(l.map(_.tail)))
    l.foldRight(init) { case (elem, acc) => Inf(elem.head)(acc) }
  }

  def unfold[S,A](seed : S)(f : S => (A, S)) : Inf[A] = laz {
    val x = f(seed)
    Inf(x._1)(unfold(x._2)(f))
  }
}