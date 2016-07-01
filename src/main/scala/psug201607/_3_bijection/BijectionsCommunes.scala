package psug201607._3_bijection

object BijectionsCommunes {

  def optionIsEither[A] = new (Option[A] <=> Either[Unit, A]) {
    def from(f: Option[A]) : Either[Unit, A] = f match {
      case None    => Left(())
      case Some(a) => Right(a)
    }

    def to(f: Either[Unit, A]): Option[A] = f match {
      case Left(()) => None
      case Right(a) => Some(a)
    }
  }

  def const[A] = new ((Unit => A) <=> A) {
    def from(f: Unit => A): A = f(())
    def to(a: A): Unit => A = _ => a
  }


  def string  = new (List[Char] <=> String) {
    def from(a: List[Char]): String = a.mkString
    def to(b: String): List[Char] = b.toList
  }
}





