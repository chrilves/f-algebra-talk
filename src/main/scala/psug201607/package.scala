import psug201607.annexe.Entiers.{AlgebreInt, AlgebreIntEntier, AlgebreIntOps, Entier}

package object psug201607 {
  @inline implicit final def toAlgebreInt[A : AlgebreInt](self : A) : AlgebreIntOps[A] = new AlgebreIntOps[A](self)

  implicit final val algebreIntEntier : AlgebreInt[Entier] = AlgebreIntEntier
}
