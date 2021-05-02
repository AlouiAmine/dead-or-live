import junit.framework.Assert.assertEquals

class DeadOrLiveTest {


  @org.junit.Test
  def test1(): Unit = {
    val grille: String =
      """##..#...##
        |###...##..
        |..#..#..##
        |.#..#.##..""".stripMargin

    var expected = "..........\n........#.\n..##.#...#\n..#######."
    var actual = DeadOrLive.genererGrille(grille, n_iteration = 5)

    assertEquals(expected, actual)
}
  @org.junit.Test
  def Test2(): Unit = {
    val voisins : List[Char] = List('#','#','#','.')
    val cellule : Char = '#'
    val expected = '#'
    val actual = DeadOrLive.checkDeadOrLive(voisins : List[Char], cellule: Char)
    assertEquals(actual,expected)
  }
  @org.junit.Test
  def Test3(): Unit = {
    val voisins : List[Char] = List('#','#','#','.','#')
    val cellule : Char = '#'
    val expected = '.'
    val actual = DeadOrLive.checkDeadOrLive(voisins : List[Char], cellule: Char)
    assertEquals(actual,expected)
  }

  @org.junit.Test
  def Test4(): Unit = {
    val voisins : List[Char] = List('#','#','.','.','.')
    val cellule : Char = '#'
    val expected = '#'
    val actual = DeadOrLive.checkDeadOrLive(voisins : List[Char], cellule: Char)
    assertEquals(actual,expected)
  }

}
