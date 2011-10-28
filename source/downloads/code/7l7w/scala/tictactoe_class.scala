class TicTacToe() {
  val players = List("O", "X")
  val lines = List(((0,0),(1,1),(2,2)), ((0,2),(1,1),(2,0))) ++ (0 to 2).flatMap((i:Int) => List(((0,i), (1,i), (2,i)), ((i,0),(i,1),(i,2))))

  val board = Array.ofDim(3,3):Array[Array[Char]]
  init_board()

  def get(i: (Int,Int)): Char = board(i._1)(i._2)

  def init_board() {
    for(i <- 0 to 2) {
      for(j <- 0 to 2) {
        board(i)(j) = ' '
      }
    }
  }

  def isWinner(): Boolean = {
    lines.foreach {
      case(i1,i2,i3) =>
        if (get(i1) == get(i2) && get(i2) == get(i3)) {
          if (get(i1) != ' ') {
            return true
          }
        }
    }
    return false
  }

  def print_board_with_num() {
    println("-----")
    for(i <- 0 to 2) {
      print('|')
      for (j <- 0 to 2) {
        val c = get((i,j))
        if (c == ' ')
          print(i*3+j+1)
        else
          print(c)
      }
      println('|')
    }
    println("-----")
  }

  def play() {
    (1 to 9).foreach { move =>
      if (play_one_move(players(move % 2)))
        return
    }

    println("Draw!")
  }

  def play_one_move(player: String): Boolean = {
    print_board_with_num()
    val (i, j) = get_move(player)
    board(i)(j) = player(0)
    if (isWinner) {
      println("Player " + player + " won!")
      return true
    }

    return false
  }

  def get_move(player: String): (Int, Int) = {
    while(true) {
      print("Player " + player)
      print(" move: ")
      val ln = readLine().toInt
      val i = (ln -1 ) / 3
      val j = (ln - 1) % 3
      if (board(i)(j) != ' ')
        println("Invalid position. Try again")
      else 
        return (i,j)
    }
    return (-1,-1)                      // actually, can't happen but compiler doesn't know
  }
}

val game = new TicTacToe
game.play()
