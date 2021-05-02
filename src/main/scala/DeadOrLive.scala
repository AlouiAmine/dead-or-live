import scala.Console.println
import scala.collection.mutable.ListBuffer

object DeadOrLive {

  def get_voisin(largeur: Int, longueur: Int, grille: List[String], x_idx: Int, y_idx: Int): List[Char] = {
    var voisins = List[Char]()
    if (x_idx == 0) {
      val row_1: String = grille(0)
      val row_2: String = grille(1)

      if (y_idx ==0){
        voisins  = List(row_1(y_idx + 1), row_2(y_idx), row_2(y_idx + 1))
      } else if (y_idx == largeur - 1){
        voisins  = List(row_1(y_idx - 1), row_2(y_idx), row_2(y_idx - 1))
      } else {
          voisins = List(row_1(y_idx - 1), row_1(y_idx + 1), row_2(y_idx), row_2(y_idx - 1), row_2(y_idx + 1))
      }
    }
    else if (x_idx == longueur - 1) {
      val row_1: String = grille(x_idx)
      val row_2: String = grille(x_idx - 1)

      if (y_idx ==0){
        voisins = List(row_1(y_idx + 1), row_2(y_idx), row_2(y_idx + 1))
      } else if (y_idx == largeur - 1){
        voisins = List(row_1(y_idx - 1), row_2(y_idx), row_2(y_idx - 1))
      } else {
        voisins = List(row_1(y_idx - 1),row_1(y_idx + 1), row_2(y_idx), row_2(y_idx - 1), row_2(y_idx + 1))
      }

    }else {
      val row_1: String = grille(x_idx)
      val row_2: String = grille(x_idx - 1)
      val row_3: String = grille(x_idx + 1)

      if (y_idx ==0){
        voisins = List(row_1(y_idx + 1), row_2(y_idx),row_3(y_idx), row_2(y_idx + 1), row_3(y_idx + 1))
      } else if (y_idx == largeur - 1){
        voisins = List(row_1(y_idx - 1), row_2(y_idx),row_3(y_idx), row_2(y_idx - 1),row_3(y_idx - 1))
      } else {
        voisins = List(row_1(y_idx - 1),row_1(y_idx + 1), row_2(y_idx),row_3(y_idx), row_2(y_idx - 1),row_3(y_idx - 1), row_2(y_idx + 1), row_3(y_idx + 1))
      }
    }
    voisins
  }
  def dead_or_live(voisins : List[Char], cellule: Char) : Char = {
    var n_vivantes = 0
    val occurence_map  = voisins.groupBy(identity).mapValues(_.size)
    var cellule_t1 = ' '
    try {
      n_vivantes = occurence_map('#')
    } catch {
      case exception: NoSuchElementException =>
    }
    if (cellule == '#') {
      if (n_vivantes < 2) {
        cellule_t1 = '.'
      }
      else if ((n_vivantes == 2) | (n_vivantes == 3)) {
        cellule_t1  = '#'
      }
      else {
        cellule_t1  = '.'
      }
    } else {
      if (n_vivantes == 3) {
        cellule_t1 = '#'
      } else {
        cellule_t1 = '.'
      }
    }
  cellule_t1
  }
  def evolution_grille(grille : String, n_iteration : Int): String = {


    var grille_ls: List[String] = grille.split( "\n").map(_.trim).toList
    val largeur: Int = grille_ls(0).length
    val longueur: Int = grille_ls.length

    for (k <- 1 to n_iteration) {
      var output = new ListBuffer[String]()

      for (i <- 0 to (longueur - 1)){
        var row : String = ""
        for (j <- 0 to (largeur - 1)) {
          val voisins : List[Char] = get_voisin(largeur, longueur, grille_ls, i,j)
          row += dead_or_live(voisins, grille_ls(i)(j))
        }
        output += row

      }
      grille_ls = output.toList
    }
    grille_ls.mkString("\n")
}
  def main(args: Array[String]): Unit = {

    val n_iteration : Int = 5
    val grille: String =
      """##..#...##
        |###...##..
        |..#..#..##
        |.#..#.##..""".stripMargin

    println("Input: ")
    println(grille)
    println("nombre d'itération  : " + n_iteration)
    println("Output: ")
    println(evolution_grille(grille, n_iteration))

  }

}
