masterBoard <- array(0, dim = c(3,3,3,3))
boardStatus <- matrix(0, nrow = 3, ncol = 3, byrow = T)

hasWonBoard <- function(board,player){
  hasWon <- F
  dimension <- length(board[1,])
  
  #First check horizontal
  for(row in 1:dimension){
    if(board[row,1] == board[row,2] && board[row,2] == board[row,3] && board[row,2] == player){
      hasWon <- T
      break
    }
  }
  if(hasWon)
    return(T)
  
  #Check vertical
  for(col in 1:dimension){
    if(board[1,col] == board[2,col] && board[2,col] == board[3,col] && board[2,col] == player){
      hasWon <- T
      break
    }
  }
  if(hasWon)
    return(T)
  
  #Check the diagonals
  if(board[1,1] == board[2,2] && board[2,2] == board[3,3] && board[2,2] == player)
    return(T)
  if(board[3,1] == board[2,2] && board[2,2] == board[1,3] && board[2,2] == player)
    return(T)
  
  return(F)
}

# Return a list of valid moves
getValidMove <- function(board, forcedMove, boardStatus){
  validMove <- list()
  
  # Check if the subBoard is already won by a player
  if (boardStatus[forcedMove[1], forcedMove[2]] == 0){
    for (subX in 1:3)
      for (subY in 1:3)
        if (board[subX, subY, forcedMove[1], forcedMove[2]] == 0)
          validMove <- append(validMove, list(c(subX, subY, forcedMove[1], forcedMove[2])))  # Add the valid move
  }
  else
    for (x in 1:3)
      for (y in 1:3)
        if (boardStatus[x, y] == 0)
          for (subX in 1:3)
            for (subY in 1:3)
              if (board[subX, subY,x, y] == 0)
                validMove <- append(validMove, list(c(subX, subY, x, y)))  # Add the valid move
              
              return (validMove)
}

#player move on master board
doMove <- function(position,player,board){
  if(board[postion[1],position[2],position[3],position[4]] == 0){
    board[postion[1],position[2],position[3],position[4]] <- player#Ask MD about positions
  }
  return(board)
}
















