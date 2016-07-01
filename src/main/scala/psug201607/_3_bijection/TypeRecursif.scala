package psug201607._3_bijection


trait TypeRecursif {

  sealed abstract class TypeRecursifP
  final case class SansFinP(valeur : TypeRecursifP) extends TypeRecursifP

  trait TypeRecursifI {
    def fold[R](f : R => R) : R
  }

  def SansFinI(valeur : TypeRecursifI) = new TypeRecursifI {
    def fold[R](f : R => R) : R = valeur.fold(f)
  }

  def bijectionTypeRecursif : TypeRecursifP <=> TypeRecursifI =
    new (TypeRecursifP <=> TypeRecursifI) {

      def from(a: TypeRecursifP): TypeRecursifI = a match {
        case SansFinP(v) => SansFinI(bijectionTypeRecursif.from(v))
      }

      def to(b: TypeRecursifI): TypeRecursifP = b.fold(SansFinP)
    }
}
