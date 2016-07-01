package psug201607

import org.scalatest.FunSuite
import psug201607.annexe.BooleanAlgebraInstances
import psug201607.annexe.BooleanAlgebraInstances.Couleur
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.scalacheck.Shapeless._
import psug201607._5_morphisme.Num
import _3_bijection.BijectionsCommunes._
import _3_bijection.Produit._
import _3_bijection.Somme._
import _8_lois.Algebre._
import psug201607._7_liberte._
import psug201607.annexe.Entiers.AlgebreIntEntier
import Lib._
import psug201607._4_algebre.Document
import _4_algebre.Document._
import _6_initiale_finale.Initiale._

class Tests extends FunSuite with org.typelevel.discipline.scalatest.Discipline {
  import org.scalacheck._

  //////////////////// 3 - Bijections ////////////////////////

  checkAll("Option[Int]  <=> Either[Unit, Int]"  , optionIsEither[Int].laws.iso)
  checkAll("String       <=> List[Char]"         , string.laws.iso)
  checkAll("VoitureP     <=> VoitureI"           , bijectionVoiture.laws.iso)
  checkAll("BooleenP     <=> BooleenI"           , bijectionBooleen.laws.iso)


  //////////////////// 4 - (Co-)Algebres /////////////////////


  checkAll("HTML"    , implicitly[Document[HTML]].lois.document)
  checkAll("MarkDown", implicitly[Document[MarkDown]].lois.document)

  //////////////////// 5 - Moorhismes ////////////////////////


  checkAll("taille" , Num.taille.lois.morphisme1)
  checkAll("taille" , Num.taille.lois.morphisme2)

  //////////////////// 6 - Initiale /////////////////////////

  checkAll("IntialPSUG_P <=> InitialPSUG_I", isoMorphisme_P_I.lois.isoMorphisme)
  checkAll("IntialPSUG_I <=> InitialPSUG_F", isoMorphisme_I_F.lois.isoMorphisme)
  checkAll("IntialPSUG_F <=> InitialPSUG_P", isoMorphisme_F_P.lois.isoMorphisme)

  //////////////////// 7 - Libertée /////////////////////////

  checkAll("LibrePSUG_P <=> LibrePSUG_I", isoMorphismeAlgebre_P_I.lois.isoMorphisme)

  //////////////////// 8 - Lois /////////////////////////////

  checkAll("MonoidLibreP", MonoidLibreP.monoid.lois.monoid)
  checkAll("MonoidLibreI", MonoidLibreI.monoid.lois.monoid)

  checkAll("MonoidLibreP <=> MonoidLibreI", isoMorphismeMonoid_I_P.lois.isoMorphisme)

  //////////////////// Entiers ///////////////////////////////

  checkAll("Algebre Entier" , AlgebreIntEntier.laws.algebre)


  //////////////////// BOOLEENS //////////////////////////////

  implicit val artitrarySetCouleur : Arbitrary[Set[Couleur]] = Arbitrary(Gen.containerOf[Set, Couleur](Gen.oneOf(Couleur.all.toSeq)))
  implicit val artitrarySeqCouleur : Arbitrary[Seq[Couleur]] = Arbitrary(Gen.containerOf[Seq, Couleur](Gen.oneOf(Couleur.all.toSeq)))

  checkAll("Boolean      is boolean algebra", BooleanAlgebraInstances.booleanBool.laws.boolean)
  checkAll("Set[Couleur] is boolean algebra", BooleanAlgebraInstances.setCouleur.laws.boolean)
  //checkAll("Seq[Couleur] is boolean algebra", BooleanAlgebraInstances.seqCouleur.laws.boolean)
}
