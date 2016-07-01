package psug201607._3_bijection

import org.scalacheck.Arbitrary

object Produit {

  // Case Class

  final case class VoitureP(model : String, portes : Int, couleur : String)

  // Double negation

  trait VoitureI {
    def matcher[R](consomateur : (String, Int, String) => R) : R

    override def equals(o : Any) : Boolean = o match {
      case v : VoitureI =>
        val f = (a : String,b : Int,c : String) => (a,b,c)
        matcher(f) == v.matcher(f)
      case _ => false
    }

    override def hashCode = matcher[(String, Int, String)]((a,b,c) => (a,b,c)).hashCode
  }

  object VoitureI {

    def apply(model : String, portes : Int, couleur : String) = new VoitureI {
      def matcher[R](f: (String, Int, String) => R): R = f(model, portes, couleur)
    }

    implicit val arbitraryVoitureI  : Arbitrary[VoitureI] = Arbitrary {
      for {
        model   <- implicitly[Arbitrary[String]].arbitrary
        portes  <- implicitly[Arbitrary[Int]].arbitrary
        couleur <- implicitly[Arbitrary[String]].arbitrary
      } yield VoitureI(model, portes, couleur)
    }
  }

  // Matching

  def description(model : String, portes : Int, couleur : String) : String = s"Une $model $couleur à $portes portes."

  val desciptionP = VoitureP("Corsa",3,"rouge") match {
    case VoitureP(m, p, c) => description(m,p,c)
  }

  val descriptionI = VoitureI("Corsa", 3, "rouge").matcher {
    (m, p, c) => description(m,p,c)
  }


  // Isomorphisme

  final val bijectionVoiture = new (VoitureP <=> VoitureI) {
    def from(v: VoitureP): VoitureI = VoitureI(v.model, v.portes, v.couleur)
    def to(v: VoitureI): VoitureP = v.matcher(VoitureP.apply _)
  }
}