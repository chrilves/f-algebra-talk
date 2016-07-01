package psug201607._4_algebre


object Algebre {


  // Algebre

  trait AlgebrePSUG[A] extends (OperationPSUG[A] => A) {
    def zero                        : A
    def plus(gauche : A, droit : A) : A

    final def apply(opa : OperationPSUG[A]) : A = opa match {
      case Zero      => zero
      case Plus(g,d) => plus(g,d)
    }
  }

  object AlgebrePSUG {
    def apply[A](algebre : OperationPSUG[A] => A) = new AlgebrePSUG[A] {
      def zero                     : A = algebre(Zero)
      def plus(gauche:A, droite:A) : A = algebre(Plus(gauche, droite))
    }
  }

  sealed abstract class OperationPSUG[+A] {
    def map[B](f : A => B) : OperationPSUG[B] = this match {
      case Zero      => Zero
      case Plus(g,d) => Plus(f(g), f(d))
    }
  }
  case object Zero                            extends OperationPSUG[Nothing]
  case class  Plus[+A](gauche : A, droit : A) extends OperationPSUG[A]

  def traitVersFonction[A](t : AlgebrePSUG[A]       ) : OperationPSUG[A] => A = t
  def fonctionVersTrait[A](f : OperationPSUG[A] => A) : AlgebrePSUG[A]        = AlgebrePSUG(f)


  //
  //  Unit
  //


  object UnitPSUG extends AlgebrePSUG[Unit] {
    def zero: Unit = ()
    def plus(gauche: Unit, droit: Unit): Unit = ()
  }

  assert(UnitPSUG.zero.==( () ))
  assert(UnitPSUG.plus( () , () ).==(()))

  def algebrePSUGUnit(op : OperationPSUG[Unit]) : Unit = op match {
    case Zero      => ()
    case Plus(g,d) => ()
  }

  algebrePSUGUnit(Zero).==(())
  algebrePSUGUnit(Plus( (), () )).==(())


  //
  //  Int
  //

  object IntPSUG extends AlgebrePSUG[Int] {
    def zero: Int = 0
    def plus(gauche: Int, droit: Int): Int = gauche + droit
  }

  assert(IntPSUG.zero == 0 )
  assert(IntPSUG.plus( 37 , 82 ) == 119)

  def algebrePSUGInt(op : OperationPSUG[Int]) : Int = op match {
    case Zero      => 0
    case Plus(g,d) => g + d
  }

  algebrePSUGInt(Zero) == 0
  algebrePSUGInt(Plus(37, 82)) == 119


  //
  // Listes
  //

  class ListPSUG[A] extends AlgebrePSUG[List[A]] {
    def zero: List[A] = Nil
    def plus(gauche: List[A], droit: List[A]): List[A] = gauche ++ droit
  }

  assert(new ListPSUG[Int].zero == Nil)
  assert(new ListPSUG[Int].plus( List(1,2) , List(3,4) ) == List(1,2,3,4) )

  def algebrePSUGList[A](op : OperationPSUG[List[A]]) : List[A] = op match {
    case Zero      => Nil
    case Plus(g,d) => g ++ d
  }

  algebrePSUGList(Zero) == Nil
  algebrePSUGList(Plus(List(1,2), List(3,4))) == List(1,2,3,4)

  //
  // Fonctions
  //

  object FunPSUG extends AlgebrePSUG[Int => Int] {
    def zero : Int => Int = (x => x)
    def plus(g : Int => Int, d : Int => Int) : Int => Int = (x => d(g(x)))
  }

  val valeur_de_test = 66

  assert(FunPSUG.zero(valeur_de_test) == ({ x :Int => x })(valeur_de_test))
  assert(FunPSUG.plus( (x:Int)=>x+1 , (x:Int)=>x*3 )(valeur_de_test) == ({ x : Int => (x+1)*3 })(valeur_de_test))

  def algebrePSUGFun(op : OperationPSUG[Int => Int]) : Int => Int = op match {
    case Zero      => (x => x)
    case Plus(g,d) => (x => d(g(x)))
  }

  algebrePSUGFun(Zero)(valeur_de_test) == ({ x : Int => x })(valeur_de_test)
  algebrePSUGFun(Plus( (x:Int)=>x+1 , (x:Int)=>x*3 ))(valeur_de_test) == ({ x : Int => (x+1)*3 })(valeur_de_test)
}
