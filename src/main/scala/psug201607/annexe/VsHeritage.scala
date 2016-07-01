package psug201607.annexe


object VsHeritage {

  // Algebre

  trait AlgebrePlus[A] {
    def plus(l : A, r : A) : A
  }

  implicit val intPlus     = new AlgebrePlus[Int]     { def plus(l : Int, r : Int    )     : Int     = l +  r}
  implicit val stringPlus  = new AlgebrePlus[String]  { def plus(l : String, r : String )  : String  = l +  r}
  implicit def listPlus[A] = new AlgebrePlus[List[A]] { def plus(l : List[A], r : List[A]) : List[A] = l ++ r}

  def double[A : AlgebrePlus](x : A) : A = implicitly[AlgebrePlus[A]].plus(x, x)


  // Interface/Classe

  trait TraitPlus {
    def plus(r : TraitPlus) : TraitPlus
  }

  final case class IntPlus1(value : Int) extends TraitPlus {
    def plus(r : TraitPlus) : TraitPlus = ???
  }

  final case class StringPlus1(value : String) extends TraitPlus {
    def plus(r : TraitPlus) : TraitPlus = ???
  }


  // F-Bounded

  trait TraitPlusFBounded[A <: TraitPlusFBounded[A]] {
    def plus(r : A) : A
  }

  final case class IntPlus2(value : Int) extends TraitPlusFBounded[IntPlus2] {
    def plus(r : IntPlus2) : IntPlus2 = IntPlus2(value + r.value)
  }

  def doubleF[A <: TraitPlusFBounded[A]](a : A) : A = a.plus(a)


  // Quid des constantes ??


  trait AlgebreZero[A] {
    def zero : A
  }

  implicit val intZero     = new AlgebreZero[Int]     { def zero = 0   }
  implicit val stringZero  = new AlgebreZero[String]  { def zero = ""  }
  implicit def listZero[A] = new AlgebreZero[List[A]] { def zero = Nil }

  // Quid des tests?

  // Factory!
}
