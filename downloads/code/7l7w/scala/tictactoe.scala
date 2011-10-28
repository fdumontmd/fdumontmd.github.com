val board = Array.ofDim(3,3):Array[Array[Char]]
val lines = List(((0,0),(1,1),(2,2)), ((0,2),(1,1),(2,0))) ++ (0 to 2).flatMap((i:Int) => List(((0,i), (1,i), (2,i)), ((i,0),(i,1),(i,2))))

def get(b: Array[Array[Char]], i: (Int,Int)): Char = b(i._1)(i._2)


def winner(b: Array[Array[Char]]): Boolean = {
  lines.foreach {
    case(i1,i2,i3) =>
     if (get(b,i1) == get(b, i2) && get(b, i2) == get(b, i3)) {
       if (get(b, i1) == 'X') {
         println("Player X is the winner");
         return true
       } else if (get(b, i1) == 'O') {
         println("Player Y is the winner");
         return true
       }
     }
  }
  return false
}

def print_board(b: Array[Array[Char]]) {
  println("-----")
  for(i <- 0 to 2) {
    print('|')
    for (j <- 0 to 2) {
      val c = get(b, (i,j))
      if (c == ' ')
        print(i*3+j+1)
      else
        print(c)
    }
    println('|')
  }
  println("-----")
}


def print_board_clear(b: Array[Array[Char]]) {
  println("-----")
  b.foreach {
    line =>
      print('|')
      line.foreach { 
      c => print(c)
    }
    println('|')
  }
  println("-----")
}

def init_board(b: Array[Array[Char]]) {
  for(i <- 0 to 2) {
    for(j <- 0 to 2) {
      b(i)(j) = ' '
    }
  }
}

init_board(board)

println("Test on empty board")
winner(board)

print_board(board)
board(1)(1)='X'
print_board(board)
board(0)(0)='O'
print_board(board)
board(0)(2)='X'
print_board(board)
board(0)(1)='O'
print_board(board)
board(2)(0)='X'
print_board(board)

println("After game:")
winner(board)

println("And now play:")
init_board(board)

def play(b: Array[Array[Char]]) {
  init_board(b)
  var playerX = true
  (1 to 9).foreach { i =>
    print_board(b)
    print(i + " ")
    var valid_pos = false
    while(!valid_pos) {
      if (playerX)
        print("Player X")
      else
        print("Player O")
      println(" move:")
      val ln = readLine().toInt
      val i = (ln -1 ) / 3
      val j = (ln - 1) % 3
      if (b(i)(j) != ' ')
        println("Invalid position. Try again")
      else {
        if (playerX)
          b(i)(j) = 'X'
        else
          b(i)(j) = 'O'

        valid_pos = true
      }
    }

    playerX = !playerX
    
    if(winner(b))
      return

  }

  println("Draw!")
}

play(board)
