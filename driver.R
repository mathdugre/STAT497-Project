# Driver

source("model.r")

# Start game
masterBoard <- array(0, dim = c(3,3,3,3))
statusBoard <- matrix(0, nrow = 3, ncol = 3, byrow = T)
winner <- 0  # Initially a tie

# First move
player <- 1
move <- as.numeric(unlist(strsplit(readline("Enter a move: "), ",")));
masterBoard[move[1], move[2], move[3], move[4]] <- player
forcedMove <- c(move[1], move[2])
player <- player %% 2 + 1

# Debug
cat("--------------------------\n")
printMasterBoard(masterBoard)

# The game continue normally
while (T) {
  validMoves <- getValidMove(masterBoard, forcedMove, statusBoard)
  
  if (length(validMoves) == 0)
    return (0)
  
  # Some smart method to determine a move instead
  move <- as.numeric(unlist(strsplit(readline("Enter a move: "), ",")));
  
  # Play  the move
  masterBoard[move[1], move[2], move[3], move[4]] <- player
  forcedMove <- c(move[1], move[2])
  
  # Check for win
  if(hasWonBoard(masterBoard[move[1], move[2],,], player))
    statusBoard[move[1], move[2]] <- player  # Won subBoard
  if(hasWonBoard(statusBoard, player))
    return (player)  # Won game
  
  player <- player %% 2 + 1  # Change player
  
  # Debug
  cat("--------------------------\n")
  printMasterBoard(masterBoard)
}