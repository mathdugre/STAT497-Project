# this agent plays randomly against another random agent
simulUTTT <- function(theseed){
  # To reproduce experiment
  set.seed(theseed)
  
  # Start game
  masterBoard <- array(0, dim = c(3,3,3,3))
  statusBoard <- matrix(0, nrow = 3, ncol = 3, byrow = T)
  winner <- 0  # Initially a tie
  
  # First move
  player <- 1
  
  # random method
  move <- sample(c(1,2,3), size = 4, replace = T)
  masterBoard[move[1], move[2], move[3], move[4]] <- player
  forcedMove <- c(move[3], move[4])
  player <- player %% 2 + 1
  
  # The game continue normally
  while (T) {
    validMoves <- getValidMove(masterBoard, forcedMove, statusBoard)
    
    if (length(validMoves) == 0)
      break
    
    # random move method
    move <- sample(validMoves, size = 1)
    move <- move[[1]]
    
    # Play  the move
    masterBoard[move[1], move[2], move[3], move[4]] <- player
    forcedMove <- c(move[3], move[4])
    
    # Check for win
    if(hasWonBoard(masterBoard[move[1], move[2],,], player))
      statusBoard[move[1], move[2]] <- player  # Won subBoard
    if(hasWonBoard(statusBoard, player)){
      winner <- player # Won game
      break 
    }
    
    player <- player %% 2 + 1  # Change player
  }
  return(winner)
}
# simulate for playing first and second

# playing as player 1 - randomly

winsP1 <- 0 
lossesP1 <- 0
drawsP1 <- 0
nepis = 10000

winRatioAs1 <- vector("numeric",length = nepis)

for(i in 1:nepis){
  return <- simulUTTT(i)
  
  if (return == 0)
    drawsP1 <- drawsP1 + 1
  else if(return == 1)
    winsP1 <- winsP1 + 1
  else
    lossesP1 <- lossesP1 + 1
  
  winRatioAs1[i] = winsP1/i
}

# playing as player 2 - randomly

winsP2 <- 0
lossesP2 <- 0
drawsP2 <- 0

winRatioAs2 <- vector("numeric",length=nepis)

for(i in 1:nepis){
  return <- simulUTTT(i+nepis)
  
  if (return == 0)
    drawsP2 <- drawsP2 + 1
  else if(return == 2)
    winsP2 <- winsP2 + 1
  else
    lossesP2 <- lossesP2 + 1
  
  winRatioAs2[i] <- winsP2/i
}

# plot the win ratio as player 1 and 2
plot(1:nepis,winRatioAs1, type = "l")
lines(winRatioAs2)


