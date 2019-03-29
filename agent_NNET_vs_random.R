

# convert board to a vector for the nnet
boardToVector <- function(board){
  # take elments in board an lists them from left to right, top to bottom as a vector
  vec <- c()
  for(i in 1:3){
    for(j in 1:3){
      vec <- c(vec, board[i,1,j,], board[i,2,j,], board[i,3,j,])
    }
  }
  return(vec)
}

# reward function
reward <- function(winner, playingAs){
  # returns a reward of -1,0,1 based on the winner + who you are playing as
  if(winner == 0)
    return(0) # a draw always has reward 0
  else if(winner == 1 && playingAs == 1)
    return(1)
  else if(winner == 1 && playingAs == 2)
    return(-1)
  else if(winner == 2 && playingAs == 1)
    return(-1)
  else
    return(1)
}

# simulate with states
simulUTTT <- function(theseed){
  # To reproduce experiment
  set.seed(theseed)
  
  # Start game
  masterBoard <- array(0, dim = c(3,3,3,3))
  statusBoard <- matrix(0, nrow = 3, ncol = 3, byrow = T)
  winner <- 0  # Initially a tie
  
  # track the states 
  boardStates <- matrix(0, ncol = 81)
  
  # First move
  player <- 1
  
  # random method
  move <- sample(c(1,2,3), size = 4, replace = T)
  masterBoard[move[1], move[2], move[3], move[4]] <- player
  forcedMove <- c(move[3], move[4])
  player <- player %% 2 + 1
  
  # add state to tracker
  boardStates <- rbind(boardStates, boardToVector(masterBoard))
  
  # The game continue normally
  while (T) {
    validMoves <- getValidMove(masterBoard, forcedMove, statusBoard)
    
    if (length(validMoves) == 0)
      break
    
    move <- sample(validMoves, size = 1)
    move <- move[[1]]
    
    # Play  the move
    masterBoard[move[1], move[2], move[3], move[4]] <- player
    forcedMove <- c(move[3], move[4])
    
    # Update tracked states
    boardStates <- rbind(boardStates, boardToVector(masterBoard))
    
    # Check for win
    if(hasWonBoard(masterBoard[move[1], move[2],,], player))
      statusBoard[move[1], move[2]] <- player  # Won subBoard
    if(hasWonBoard(statusBoard, player)){
      winner <- player # Won game
      break 
    }
    
    player <- player %% 2 + 1  # Change player
  }

  return(list(winner,boardStates))
}


# create a dataset that contains runs of random plays (states) + of the winner (rewards)
nepis <- 10000
RSDataAs1 <- rep(NULL, nepis)

# playing as player 1
for(i in 1:nepis){
  run <- simulUTTT(i)
  rewardR <- reward(run[[1]],playingAs = 1)
  RSDataAs1[[i]] <- list(rewardR,run[[2]])
}

RSDataAs2 <- rep(NULL,nepis)

# playing as player 2
for(i in 1:nepis){
  run <- simulUTTT(i+nepis)
  rewardR <- reward(run[[1]],playingAs = 2)
  RSDataAs2[[i]] <- list(rewardR,run[[2]])
}

# to access the notation is
# rewards: RSDataAs1[[episode #]][[1]]
# sequence of states: RSDaraAs1[[episode #]][[2]][row #,]
# to get a quick look at the dimension of any given episode matrix use dim(RSData[[ep #]][[2]])

# Encode into a neural network input form

stateToInLayer <- function(state){
  # takes a state vector (sequence of 0,1 and 2's) and returns vector of bits of 3*81 long
  # for player 1, player 2 and unoccupied (i.e  0)
  stateLayer <- vector("numeric", 3*81)
  seqlen <- 81
  
  for(i in 1:seqlen){
    if(state[i] == 0){
      stateLayer[i+2*seqlen] <- 1
    }
    else if(state[i] == 1){
      stateLayer[i] <- 1
    }
    else{
      stateLayer[i+seqlen] <- 1
    }
  }
  
  return(stateLayer)
}

# extract x and y values for nnet
yDataAs1 <- vector("numeric", nepis)
yDataAs2 <- vector("numeric", nepis)

for(i in 1:nepis){
  yDataAs1[i] <- RSDataAs1[[i]][[1]]
  yDataAs2[i] <- RSDataAs2[[i]][[1]]
}

xDataAs1 <- rep(NULL,nepis) # will become list of encoded matrices
xDataAs2 <- rep(NULL,nepis)

for(i in 1:nepis){
  dim <- dim(RSDataAs1[[i]][[2]])[1] # number of rows in matrix episode
  xDataAs1[[i]] <- matrix(0, nrow = dim, ncol = 3*81, byrow = T)
  for(j in 1:dim){
    xDataAs1[[i]][j,] <- stateToInLayer(RSDataAs1[[i]][[2]][j,])
  }
  dim <- dim(RSDataAs2[[i]][[2]])[1] # number of rows in matrix episode
  xDataAs2[[i]] <- matrix(0, nrow = dim, ncol = 3*81, byrow = T)
  for(j in 1:dim){
    xDataAs2[[i]][j,] <- stateToInLayer(RSDataAs2[[i]][[2]][j,])
  }
}

library(keras)

# bulid neural network using keras
model1 <- keras_model_sequential()

# build layer for model
# input layer: 3*81 which is a sequence of bits 81 for the player 1 position, 81 for player 2 and
# 81 for the unoccupied spaces
# 3 hidden layers, 81 nodes each
# ouput layer: 3 nodes corersponding to winning, loosing or drawing (for either player)

model1 %>%
  layer_dense(units = 81,
              kernel_initializer = "uniform",
              activation = 'relu',
              input_shape = c(3*81)) %>%
  layer_dense(units = 81,
              kernel_intializer = "uniform",
              activation = 'relu') %>%
  layer_dense(units = 81,
              kernel_initializer = "uniform",
              activation = 'relu') %>%
  layer_dense(units = 3, activation = "softmax")

# compile model
model1 %>% compile(loss = 'categorical_crossentropy',
                  optimizer = optimizer_rmsprop(),
                  metrics = c('accuracy'))

# train nnet





