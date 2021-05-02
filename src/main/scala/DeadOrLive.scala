import scala.Console.println
import scala.collection.mutable.ListBuffer

object DeadOrLive {

  def main(args: Array[String]): Unit = {

    val n_iteration: Int = 5
    val grille: String =
      """##..#...##
        |###...##..
        |..#..#..##
        |.#..#.##..""".stripMargin

    println("Input: ")
    println(grille)
    println("nombre d'itération  : " + n_iteration)
    println("Output: ")
    println(genererGrille(grille, n_iteration))
  }

  def genererGrille(grille: String, n_iteration: Int): String = {

    var grilleLs: List[String] = grille.split("\n").map(_.trim).toList
    val largeur: Int = grilleLs(0).length
    val longueur: Int = grilleLs.length

    for (k <- 1 to n_iteration) {
      val output = new ListBuffer[String]()

      for (i <- 0 to (longueur - 1)) {
        var row: String = ""
        for (j <- 0 to (largeur - 1)) {
          val voisins: List[Char] = getVoisin(largeur, longueur, grilleLs, i, j)
          row += checkDeadOrLive(voisins, grilleLs(i)(j))
        }
        output += row

      }
      grilleLs = output.toList
    }
    grilleLs.mkString("\n")
  }

  def getVoisin(largeur: Int, longueur: Int, grille: List[String], xIdx: Int, yIdx: Int): List[Char] = {
    var voisins = List[Char]()
    if (xIdx == 0) {
      val row1: String = grille(0)
      val row2: String = grille(1)

      if (yIdx == 0) {
        voisins = List(row1(yIdx + 1), row2(yIdx), row2(yIdx + 1))
      } else if (yIdx == largeur - 1) {
        voisins = List(row1(yIdx - 1), row2(yIdx), row2(yIdx - 1))
      } else {
        voisins = List(row1(yIdx - 1), row1(yIdx + 1), row2(yIdx), row2(yIdx - 1), row2(yIdx + 1))
      }
    }
    else if (xIdx == longueur - 1) {
      val row1: String = grille(xIdx)
      val row2: String = grille(xIdx - 1)

      if (yIdx == 0) {
        voisins = List(row1(yIdx + 1), row2(yIdx), row2(yIdx + 1))
      } else if (yIdx == largeur - 1) {
        voisins = List(row1(yIdx - 1), row2(yIdx), row2(yIdx - 1))
      } else {
        voisins = List(row1(yIdx - 1), row1(yIdx + 1), row2(yIdx), row2(yIdx - 1), row2(yIdx + 1))
      }

    } else {
      val row1: String = grille(xIdx)
      val row2: String = grille(xIdx - 1)
      val row3: String = grille(xIdx + 1)

      if (yIdx == 0) {
        voisins = List(row1(yIdx + 1), row2(yIdx), row3(yIdx), row2(yIdx + 1), row3(yIdx + 1))
      } else if (yIdx == largeur - 1) {
        voisins = List(row1(yIdx - 1), row2(yIdx), row3(yIdx), row2(yIdx - 1), row3(yIdx - 1))
      } else {
        voisins = List(row1(yIdx - 1), row1(yIdx + 1), row2(yIdx), row3(yIdx), row2(yIdx - 1), row3(yIdx - 1), row2(yIdx + 1), row3(yIdx + 1))
      }
    }
    voisins
  }

  def checkDeadOrLive(voisins: List[Char], cellule: Char): Char = {
    var nVivantes = 0
    val occurenceMap = voisins.groupBy(identity).mapValues(_.size)
    var celluleGeneree = ' '
    try {
      nVivantes = occurenceMap('#')
    } catch {
      case exception: NoSuchElementException =>
    }
    if (cellule == '#') {
      if (nVivantes < 2) {
        celluleGeneree = '.'
      }
      else if ((nVivantes == 2) | (nVivantes == 3)) {
        celluleGeneree = '#'
      }
      else {
        celluleGeneree = '.'
      }
    } else {
      if (nVivantes == 3) {
        celluleGeneree = '#'
      } else {
        celluleGeneree = '.'
      }
    }
    celluleGeneree
  }

}
