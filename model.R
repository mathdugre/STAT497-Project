masterBoard = array(0, dim = c(3,3,3,3))
boardStatus = matrix(0, nrow = 3, ncol = 3, byrow = T)

hasWonBoard <- function(board,player){
  hasWon = F
  dimension = length(board[1,])
  
  #First check horizontal
  for(row in 1:dimension){
    if(board[row,1] == board[row,2] && board[row,2] == board[row,3] && board[row,2] == player){
      hasWon = T
      break
    }
  }
  if(hasWon)
    return(T)
  
  #Check vertical
  for(col in 1:dimension){
    if(board[1,col] == board[2,col] && board[2,col] == board[3,col] && board[2,col] == player){
      hasWon = T
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

