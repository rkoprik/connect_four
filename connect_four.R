
# CONNECT FOUR #
### RKOPRIK ####


# checks one move
one_move <- function(cells_possible) {
  cell_valid <- FALSE
  while (!cell_valid) {
    cell <- scan(what = numeric(), n = 1, quiet = TRUE)
    # Check validity of input
    if (!(cell %in% cells_possible)|length(cell) == 0) {
      cat("Column not valid. Again:")
    } else{
      cell_valid <- TRUE
    }
  }
  return(cell)
} 
# checks if all elements in a vector are equal
check_vector <- function(vector) {
  if (sum(vector) == 0) { # vector contains only 0s
    return(FALSE)
  } else if (any(vector != vector[1])) { # not all elements identical
    return(FALSE)
  } else { # all elements identical
    return(TRUE)
  }
}
# check win for iterating subgrids of 4x4
check_win <- function(board, n_row, n_col) {
  win <- FALSE
  # if winning min is different than 4, could be added as an extra argument 
  max_i <- n_row - 4 + 1
  max_j <- n_col - 4 + 1
  
  for (i in 1:max_i) {
    for (j in 1:max_j) {
      subgrid <- board[i:(i + 4 - 1), j:(j + 4 - 1)]
      
      if (any(apply(subgrid, 1, check_vector)) | # checks for win in rows
          any(apply(subgrid, 2, check_vector)) | # checks for win in columns
          check_vector(diag(subgrid)) | # checks for win in diagonal
          check_vector(diag(subgrid[, rev(seq_len(ncol(subgrid)))]))
      ) 
        
      { # checks for win in opposite diagonal
        win <- TRUE
      }
    }
  }
  return(win)
}
# checks the highest row possible to play - for marking the move on the grid
check_hightest_poss <- function(board, n_row, n_col) {
  poss <- matrix(NA, nrow = 1, ncol = n_col)
  dif <- n_col - n_row
  
  for (j in 1:n_col) {
    c <- board[,j]
    
    
    count <- 1
    for (i in 1:n_row) {
      
      if (c[i] != 0) 
        
        count <- count +1
    }
    poss[j] <- count
  }
  
  return(poss)
  
}
# checks the columns still available to pick
check_poss_col <- function(board, n_row, n_col) {
  col_poss <- matrix(NA, nrow = 1, ncol = n_col)
  
  
  for (j in 1:n_col) {
    c <- board[,j]
    
    
    count <- n_col
    for (i in 1:n_row) {
      
      if (c[i] == 0) 
        count <- count - 1
    }
    col_poss[j] <- count
    
    
    if (col_poss[1,j] < n_col) {
      col_poss[1,j] <- j
    } else {
      col_poss[1,j] <- 1000
    }
  }
  col_poss <- col_poss[which(col_poss != 1000)]
  return(col_poss)
  
}
# should work for pc moves, but not implemented (first function not working properly)
pc_move <- function(board, n_row, n_col) {
  
  # check for sequences of 3 in columns
  col_win <- FALSE
  max_i <- n_row - 3 + 1
  
  for (i in 1:max_i) {
    for (j in 1:n_col) {
      subgrid_pc <- as.matrix(board[i:(i + 3 - 1), j])
      
      if (any(apply(subgrid_pc, 2, check_vector))) 
        
      { 
        col_win <- TRUE
        break
      }
    }
  }
  if(col_win == TRUE | board[i-1,j] == 0){
    return(j)
  } else {
    return(sample(cells_possible, size = 1, replace = TRUE))
    
  }
  
}
# 2 real players
connect_four_2_players <- function(n_row, n_col, n_player = 2) {
  par_original <- par(no.readonly = TRUE)
  on.exit(par(par_original))
  # These arguments prevent the adding of extra space at the axis intervals
  par(xaxs = "i", yaxs = "i") 
  plot.new()
  # plot arbitrary plot based on n_col and n_row
  plot.window(xlim = c(0.5, n_col+0.5), ylim = c(0.5, n_row+0.5))
  grid(nx = n_col, ny = n_row, col = "black")
  box(lwd = 2)
  axis(2, at = 1:n_row, las = 1)
  axis(3, at = 1:n_col)
  title(main = "Connect Four")
  
  # random player selection
  start_move <- sample(c(1,2), size = 1)
  cat(paste0("Player ", start_move , " starts! \n"))
  
  if(start_move == 1){
    # initiates the board
    # check possible columns still free
    board <- matrix(0, nrow = n_row, ncol = n_col)
    cells_possible <- check_poss_col(board, n_row, n_col)
    move_count <- 1
    which_player  <- rep(c(1, 2), length = n_row*n_col)
    
    cat(paste0("In each move you have to choose one column.\n"))
    
    while (move_count < n_row*n_col+1) { 
      # execute one move - player 1
      cat(paste0("Player ", which_player[move_count], ":"))
      cell <- one_move(cells_possible)
      
      # get coordinates for plot and mark cell in plot
      column <- cell
      row <- check_hightest_poss(board, n_row, n_col)[1,cell]
      points(column, row, cex = n_col/n_row+4, pch = c(19, 19)[which_player[move_count]], 
             col = c("black", rgb(1,0.4,0.4))[which_player[move_count]], adj = 1)
      
      # update board matrix
      board[row, column] <- which_player[move_count]
      
      # check if the current player won (we check for a win after every move, i.e.,
      # if win == TRUE the player who performed the last move is the winner)
      win <- check_win(board, n_row, n_col) 
      if (win) { # terminate function with corresponding message if player won
        message <- paste0("Player ", which_player[move_count], " wins!")
        return(message)
      } else { # update the valid cells and the move count, if the player didn't win
        cells_possible <- check_poss_col(board, n_row, n_col) 
        
        move_count <- move_count + 1
      }
    }
    
    return("Game ends in a tie!") # When no valid cells are left, the game ends in a tie.
    
  } 
  if (start_move == 2) {
    # initiates the board
    board <- matrix(0, nrow = n_row, ncol = n_col)
    cells_possible <- check_poss_col(board, n_row, n_col)
    move_count <- 2
    which_player  <- rep(c(1, 2), length = n_row*n_col)
    
    cat(paste0("In each move you have to choose one column.\n"))
    
    while (move_count < n_row*n_col+1) { 
      # execute one move - player 2
      cat(paste0("Player ", which_player[move_count], ":"))
      cell <- one_move(cells_possible)
      
      # get coordinates for plot and mark cell in plot
      column <- cell
      row <- check_hightest_poss(board, n_row, n_col)[1,cell]
      points(column, row, cex = n_col/n_row+4, pch = c(19, 19)[which_player[move_count]], 
             col = c("black", rgb(1,0.4,0.4))[which_player[move_count]], adj = 1)
      
      # update board matrix
      board[row, column] <- which_player[move_count]
      
      # check if the current player won (we check for a win after every move, i.e.,
      # if win == TRUE the player who performed the last move is the winner)
      win <- check_win(board, n_row, n_col) 
      if (win) { # terminate function with corresponding message if player won
        message <- paste0("Player ", which_player[move_count], " wins!")
        return(message)
      } else { # update the valid cells and the move count, if the player didn't win
        cells_possible <- check_poss_col(board, n_row, n_col) 
        
        move_count <- move_count + 1
      }
    }
    
    return("Game ends in a tie!") # When no valid cells are left, the game ends in a tie.
    
  }
  
}
# 1 real vs PC
playing_against_computer <- function(n_row, n_col) {
  par_original <- par(no.readonly = TRUE)
  on.exit(par(par_original))
  # These arguments prevent the adding of extra space at the axis intervals
  par(xaxs = "i", yaxs = "i") 
  plot.new()
  # plot arbitraty grid depending on n_col and n_row
  plot.window(xlim = c(0.5, n_col+0.5), ylim = c(0.5, n_row+0.5))
  grid(nx = n_col, ny = n_row, col = "black")
  box(lwd = 2)
  axis(2, at = 1:n_row, las = 1)
  axis(3, at = 1:n_col)
  title(main = "Connect Four")
  
  
  # random player start
  start_move <- sample(c(1,2), size = 1)
  cat(paste0("Player ", start_move , " starts! \n"))
  
  if(start_move == 1){ 
    # initiates the board
    board <- matrix(0, nrow = n_row, ncol = n_col)
    cells_possible <- check_poss_col(board, n_row, n_col)
    move_count <- 1
    which_player  <- rep(c(1, 2), length = n_row*n_col)
    
    cat(paste0("In each move you have to choose one column.\n"))
    
    while (move_count < n_row*n_col+1) { 
      # execute one move - player 1
      cat(paste0("Player 1:"))
      cell <- one_move(cells_possible)
      
      # get coordinates for plot and mark cell in plot
      column <- cell
      row <- check_hightest_poss(board, n_row, n_col)[1,cell]
      points(column, row, cex = n_col/n_row+4, pch = c(19, 19)[which_player[move_count]], 
             col = c("black", rgb(1,0.4,0.4))[which_player[move_count]], adj = 1)
      
      # update board matrix
      board[row, column] <- which_player[move_count]
      
      # check if the current player won (we check for a win after every move, i.e.,
      # if win == TRUE the player who performed the last move is the winner)
      win <- check_win(board, n_row, n_col) 
      if (win) { # terminate function with corresponding message if player won
        message <- paste0("Player ", which_player[move_count], " wins!")
        return(message)
      } else { # update the valid cells and the move count, if the player didn't win
        cells_possible <- check_poss_col(board, n_row, n_col) 
        
        move_count <- move_count + 1
      }
      
      # execute one move - PC
      cell <- sample(cells_possible, size = 1, replace = TRUE)
      
      cat(paste("Player 2 chose column",cell,"\n"))
      
      # get coordinates for plot and mark cell in plot
      column <- cell
      row <- check_hightest_poss(board, n_row, n_col)[1,cell]
      points(column, row, cex = n_col/n_row+4, pch = c(19, 19)[which_player[move_count]], 
             col = c("black", rgb(1,0.4,0.4))[which_player[move_count]], adj = 1)
      
      # update board matrix
      board[row, column] <- which_player[move_count]
      
      # check if the current player won (we check for a win after every move, i.e.,
      # if win == TRUE the player who performed the last move is the winner)
      win <- check_win(board, n_row, n_col) 
      if (win) { # terminate function with corresponding message if player won
        message <- paste0("Player ", which_player[move_count], " wins!")
        return(message)
      } else { # update the valid cells and the move count, if the player didn't win
        cells_possible <- check_poss_col(board, n_row, n_col) 
        
        move_count <- move_count + 1
        
      }
      
    }
    return("Game ends in a tie!") # When no valid cells are left, the game ends in a tie.
  }
  if(start_move == 2){
    board <- matrix(0, nrow = n_row, ncol = n_col)
    cells_possible <- check_poss_col(board, n_row, n_col)
    move_count <- 1
    which_player  <- rep(c(1, 2), length = n_row*n_col)
    
    cat(paste0("In each move you have to choose one column.\n"))
    
    while (move_count < n_row*n_col+1) { 
      # execute one move - PC
      cell <- sample(cells_possible, size = 1, replace = TRUE)
      
      cat(paste("Player 2 chose column",cell,"\n"))
      
      # get coordinates for plot and mark cell in plot
      column <- cell
      row <- check_hightest_poss(board, n_row, n_col)[1,cell]
      points(column, row, cex = n_col/n_row+4, pch = c(19, 19)[which_player[move_count]], 
             col = c("black", rgb(1,0.4,0.4))[which_player[move_count]], adj = 1)
      
      # update board matrix
      board[row, column] <- which_player[move_count]
      
      # check if the current player won (we check for a win after every move, i.e.,
      # if win == TRUE the player who performed the last move is the winner)
      win <- check_win(board, n_row, n_col) 
      if (win) { # terminate function with corresponding message if player won
        message <- paste0("Player ", which_player[move_count], " wins!")
        return(message)
      } else { # update the valid cells and the move count, if the player didn't win
        cells_possible <- check_poss_col(board, n_row, n_col) 
        
        move_count <- move_count + 1
      }
      
      # execute one move
      cat(paste0("Player 1:"))
      cell <- one_move(cells_possible)
      
      # get coordinates for plot and mark cell in plot
      column <- cell
      row <- check_hightest_poss(board, n_row, n_col)[1,cell]
      points(column, row, cex = n_col/n_row+4, pch = c(19, 19)[which_player[move_count]], 
             col = c("black", rgb(1,0.4,0.4))[which_player[move_count]], adj = 1)
      
      # update board matrix
      board[row, column] <- which_player[move_count]
      
      # check if the current player won (we check for a win after every move, i.e.,
      # if win == TRUE the player who performed the last move is the winner)
      win <- check_win(board, n_row, n_col) 
      if (win) { # terminate function with corresponding message if player won
        message <- paste0("Player ", which_player[move_count], " wins!")
        return(message)
      } else { # update the valid cells and the move count, if the player didn't win
        cells_possible <- check_poss_col(board, n_row, n_col) 
        
        move_count <- move_count + 1
        
      }
      
    }
    return("Game ends in a tie!") # When no valid cells are left, the game ends in a tie.
  }
  
}
# start function - console will ask for inputs of players, rows and col
# inputs will be taken to the respective functions
connect_four <- function(){
  # select number of players
  cat("How many players? (1 or 2 - Type 1 to play against computer):")
  n_play_control <- scan(what = numeric(), n = 1, quiet = TRUE)
  # select rows
  cat("What's the size of the grid? Rows:")
  n_row <- scan(what = numeric(), n = 1, quiet = TRUE)
  # select col
  cat("What's the size of the grid? Columns:")
  n_col <- scan(what = numeric(), n = 1, quiet = TRUE)
  # control for number of players
  if (n_play_control == 1){
    playing_against_computer(n_row, n_col)
  }
  else if(n_play_control == 2) {
    cat("Note: empty inputs will stop the game! \n")
    connect_four_2_players(n_row, n_col, n_player = 2)  
  } else {
    message2 <- paste0("Error in connect_four(): n_player must be 1 or 2.")
    return(message2)
  }
}

connect_four()
# use connect_four() function to start
# select number of players
# select grid size
# obs: it works for any size of grid
