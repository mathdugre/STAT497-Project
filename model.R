masterBoard <- array(0, dim = c(3,3,3,3))
boardStatus <- matrix(0, nrow = 3, ncol = 3, byrow = T)

hasWonBoard <- function(board,player){
  hasWon <- F
  dimension <- length(board[1,])
  
  # First check horizontal
  for(row in 1:dimension){
    if(board[row,1] == board[row,2] && board[row,2] == board[row,3] && board[row,2] == player){
      hasWon <- T
      break
    }
  }
  if(hasWon)
    return(T)
  
  # Check vertical
  for(col in 1:dimension){
    if(board[1,col] == board[2,col] && board[2,col] == board[3,col] && board[2,col] == player){
      hasWon <- T
      break
    }
  }
  if(hasWon)
    return(T)
  
  # Check the diagonals
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

# player move on master board
doMove <- function(position,player,board){
  if(board[postion[1],position[2],position[3],position[4]] == 0){
    board[postion[1],position[2],position[3],position[4]] <- player#Ask MD about positions
  }
  return(board)
}

# prints single individual tile of TTT
printBoard <- function(board){
  for(i in 1:3){
    cat(board[i,1]," | ",board[i,2]," | ",board[i,3],"\n")
    if(i != 3){
      cat("--------------\n")
    }
  }
}

# removes first instance of element from a list and returns that list
removeElt <- function(list,elt){
  index <- 1
  while(index <= length(list)){
    if(list[[index]][1] == elt[1] && list[[index]][2] == elt[2]
       && list[[index]][3] == elt[3] && list[[index]][4] == elt[4])
    {
      list[[index]] <- NULL
      return(list)
    }
    index <- index + 1
  }
  return(list)
}

# note [a,b,,] -> a,b are the principal matrix indeces
printMasterBoard <- function(masterBoard){
  for(i in 1:3){
    for(j in 1:3){
      cat(c(masterBoard[i,1,j,], " | ", masterBoard[i,2,j,], " | ", masterBoard[i,3,j,],"\n"))
    }
    if(i != 3){
      cat("--------------------------\n")
    }
  }
}

# sample driver
nbMoves <- 81
listAllMoves <- vector(mode = "list", nbMoves)
index <- 1
for(i in 1:3)
  for(j in 1:3)
    for(k in 1:3)
      for(l in 1:3){
        listAllMoves[[index]] <- c(i,j,k,l)
        index <- index + 1
      }
playerTurn <- 1
print(listAllMoves)
while(length(listAllMoves) > 0){
  # random sample move - not following game rules
  move <- sample(listAllMoves, size = 1)
  
  # player 1
  if(playerTurn == 1){
    masterBoard[move[[1]][1],move[[1]][2],move[[1]][3],move[[1]][4]] <- playerTurn
    playerTurn <- 2
  }
  else{
    masterBoard[move[[1]][1],move[[1]][2],move[[1]][3],move[[1]][4]] <- playerTurn
    playerTurn <- 1
  }
  
  printMasterBoard(masterBoard)
  # remove move from list
  listAllMoves <- removeElt(listAllMoves,move[[1]])
}













