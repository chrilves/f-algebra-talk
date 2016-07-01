package psug201607._4_algebre

import java.net.URL

import org.typelevel.discipline
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop._

object Document {

  // L'algebre

  trait Document[A] {
    def vide                     : A
    def text(s : String)         : A
    def italique(doc : A)        : A
    def fusion(elememts : A*)     : A
    def lien(url : java.net.URL) : A
    def chapitre(titre : String) : A
    def tableDesMatieres         : A

    def lois(implicit A : Arbitrary[A]) = new discipline.Laws {
      def document = new SimpleRuleSet("document",
        "block()"       -> Prop( fusion() == vide ),
        "block(a)"      -> forAll( (a:A)     => fusion(a) == a),
        "block(text..)" -> forAll( (a:String,b:String) => fusion(text(a), text(b)) == text(a ++ b))
      )
    }
  }

  // HTML

  final case class HTML(val contenu : String) extends AnyVal

  object HTML {
    implicit final val documentHTML = new Document[HTML] {
      def vide: HTML = HTML("")
      def text(s: String): HTML = HTML(s.replaceAll("é", "&;cute"))
      def italique(doc : HTML) : HTML = HTML(s"<i>${doc.contenu}</i>")
      def fusion(elememts: HTML*): HTML = HTML(elememts.map(_.contenu).mkString)
      def tableDesMatieres: HTML = HTML("")
      def lien(url: URL): HTML = HTML(s"<a>${url.toString}</a>")
      def chapitre(titre: String): HTML = HTML(s"<h1>$titre</h1>")
    }
  }

  // Markdown

  final case class MarkDown(val contenu: String) extends AnyVal

  object MarkDown {
    implicit val documentMarkDown = new Document[MarkDown] {
      def vide: MarkDown = MarkDown("")
      def text(s: String): MarkDown = MarkDown(s)
      def italique(doc: MarkDown): MarkDown = MarkDown(s"*${doc.contenu }*")
      def fusion(elememts: MarkDown*): MarkDown = MarkDown(s"${elememts.map(_.contenu).mkString}")
      def tableDesMatieres: MarkDown = MarkDown("")
      def lien(url: URL): MarkDown = MarkDown(s"[](${url.toString })")
      def chapitre(titre: String): MarkDown = MarkDown(s"\n#$titre")
    }
  }

  // Document générique

  def livre[A](implicit A: Document[A]): A = {
    import A._

    fusion(
      tableDesMatieres,
      chapitre("Premier Chapitre"),
      fusion(
        text("le texte du premier chapitre"),
        lien(new java.net.URL("http://example.com"))
      ),
      chapitre("Deuxieme Chapitre"),
      text("Texte du deuxième chapitre")
    )
  }

  // Réalisation HTML et Markdown

  val livreHTML     = livre[HTML]
  val livreMarkDown = livre[MarkDown]
}