package psug201607

import psug201607._3_bijection.<=>
import psug201607._4_algebre.CoAlgebre._
import psug201607._5_morphisme.Num
import psug201607._5_morphisme.Num.kXp

object Main extends App {

  // 4 - Algebre

  println(s"\n${Console.YELLOW}Livre HTML${Console.RESET}")
  println(_4_algebre.Document.livreHTML.contenu)

  println(s"\n${Console.YELLOW}Livre MarkDown${Console.RESET}")
  println(_4_algebre.Document.livreMarkDown.contenu)

  // 5 - Morphisme

  println(s"\n${Console.CYAN}Benchmark Morphisme List[Any] => Int${Console.RESET}")
  println(s"  Polynome: ${Console.YELLOW}11X⁵ + 23X⁷${Console.RESET} en ${Console.GREEN}${List("true", 8, 'a')}${Console.RESET}")
  Num.benchmark(List(kXp(11L,5L), kXp(23L, 7L)))(List("true", 8, 'a'))


  // Labyrinthe
  println(s"\n\n${Console.GREEN}LABYRINTHE${Console.RESET}\n")

  val labyrinthe =
     """###########S#########
       |#           #   #   #
       |# # ######### # # ###
       |### #     #   #     #
       |# # ### ### # ##### #
       |#           #       #
       |#####################""".stripMargin

  final case class Position(x : Int, y : Int)

  final case class Joueur(labyrinthe: String, position: Position) {
    override def toString : String = {
      val map : Array[Array[Char]] = Joueur.iso.from(labyrinthe)
      map(position.y)(position.x) = 'X'
      s"Labyrinthe:\n${Joueur.iso.to(map)}\n\nPosition: ${position.x},${position.y}"
    }
  }

  object Joueur {
    def iso = new (String <=> Array[Array[Char]]) {
      def from(a: String): Array[Array[Char]] = a.lines.map(_.toArray).toArray
      def to(b: Array[Array[Char]]): String = b.map(_.mkString).mkString("\n")
    }
  }

  implicit final val joueurLabyrinthe = new Labyrinthe[Joueur] {

    def evalPos(j : Joueur) : Boolean = {
      val map : Array[Array[Char]] = Joueur.iso.from(j.labyrinthe)
      val p                        = j.position

      if ((p.y < 0) || (p.y >= map.length)) false
      else {
        val line: Array[Char] = map(p.y)

        if ((p.x < 0) || (p.x >= line.length)) false
        else {
          val c = line(p.x)
          (c != '#') && (c != '.')
        }
      }
    }

    def evalPoses(j : Joueur) : List[(String, Joueur)] =
      for {
        mov <- List(("Nord", 0,-1), ("Ouest", -1,0), ("Est", 1,0), ("Sud", 0,1))
        j2  =  (mov._1, Joueur(j.labyrinthe, Position(j.position.x + mov._2, j.position.y + mov._3))) if evalPos(j2._2)
      } yield j2

    def apply(j: Joueur): Cas[Joueur] = {
      if (!evalPos(j)) CulDeSac
      else {
        val map : Array[Array[Char]] = j.labyrinthe.lines.toArray.map(_.toArray)
        val p = j.position

        // Pos libre
        if (map(p.y)(p.x) == 'S') Sortie
          else {
            map(p.y)(p.x) = '.'

            val poses : List[(String,Joueur)] = evalPoses(Joueur(Joueur.iso.to(map), p))
            val nb    : Int          = poses.length

            nb match {
              case 0 =>
                CulDeSac
              case 1 =>
                val choix     = poses(0)
                val direction = choix._1
                val joueur    = choix._2
                println(s"Un seul chemin, vers $direction\n$joueur")
                apply(joueur)
              case _ =>
                Bifurcation(poses(0), poses(1), poses.drop(2))
            }
          }
        }
    }
  }

  def jeux[A](a : A)(implicit A : Labyrinthe[A]) : Unit = {
    println(s"Vous êtes la: \n$a\n")

    A(a) match {
      case Sortie   =>
        println(s"${Console.GREEN}Bravo vous êtes sorti!${Console.RESET}")

      case CulDeSac =>
        println(s"${Console.RED}Raté, vous êtes bloqué!${Console.RESET}")

      case Bifurcation(un, deux, l) =>
        val chemins = un :: deux :: l
        val taille  = chemins.length

        def choisir : A = {
          println(s"${Console.YELLOW}$taille chemin(s) s'offre(nt) à vous${Console.RESET}")
          for (c <- chemins.zipWithIndex) println(s"  ${c._2} : ${c._1._1}")
          print(s"${Console.YELLOW}Lequel choisissez vous? ${Console.RESET}")
          try chemins(scala.io.StdIn.readInt())._2
          catch { case _ : Throwable => choisir }
        }

        jeux(choisir)
    }
  }

  jeux(Joueur(labyrinthe, Position(15,3)))
}
