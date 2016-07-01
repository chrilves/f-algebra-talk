package psug201607._3_bijection

import org.scalacheck.{Arbitrary, Gen}

object Somme {

  // Predicatif

  sealed abstract class BooleenP

  case object VraiP extends BooleenP

  case object FauxP extends BooleenP

  // Impredicatif

  trait BooleenI {
    def matcher[R](vrai: R, faux: R): R

    override def equals(o: Any): Boolean = o match {
      case b: BooleenI =>

        val l = List(true, false)

        val res =
          for {
            x <- l
            y <- l
          } yield matcher(x, y) == b.matcher(x, y)

        res.reduce(_ && _)

      case _ =>
        false
    }

    override def hashCode: Int = matcher[Int](1, 2)
  }

  final val VraiI : BooleenI = new BooleenI {def matcher[R](vrai: R, faux: R): R = vrai}
  final val FauxI : BooleenI = new BooleenI {def matcher[R](vrai: R, faux: R): R = faux}

  object BooleenI {
    implicit final val arbitrary: Arbitrary[BooleenI] = Arbitrary(Gen.oneOf(VraiI, FauxI))
  }

  // Matching

  val si_vrai: Int = 1
  val si_faux: Int = 5

  def matching(b: BooleenP): Int = b match {
    case VraiP => si_vrai
    case FauxP => si_faux
  }

  def matching(b: BooleenI): Int = b.matcher(
    si_vrai,
    si_faux
  )

  // Bijection

  final val bijectionBooleen = new (BooleenP <=> BooleenI) {
    def from(b: BooleenP): BooleenI = b match {
      case VraiP => VraiI
      case FauxP => FauxI
    }

    def to(b: BooleenI): BooleenP = b.matcher(VraiP, FauxP)
  }
}
