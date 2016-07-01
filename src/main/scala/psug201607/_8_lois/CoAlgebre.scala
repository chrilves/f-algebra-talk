package psug201607._8_lois

import psug201607._5_morphisme.Morphisme.CoAlgebre
import psug201607._7_liberte.T

object CoAlgebre {


  sealed abstract class OperationCoAlgebreMonoid[+A]
  case object Zero                             extends OperationCoAlgebreMonoid[Nothing]
  case class  PlusElem[+A](tete: T, reste : A) extends OperationCoAlgebreMonoid[A]

  type CoAlgebreMonoid[A] = CoAlgebre[OperationCoAlgebreMonoid, A]
}
