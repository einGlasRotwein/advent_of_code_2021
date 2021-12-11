
# Get neighbours of a location in a matrix
get_neighbours <- function(mat, row, col) {
  rows <- (row-1):(row+1)
  cols <- (col-1):(col+1)
  
  neighbours <- expand.grid(row = rows, col = cols)
  
  # Exclude original value
  neighbours <-
    neighbours[!(neighbours$row == row & neighbours$col == col), ]
  
  # Exclude values outside of matrix
  neighbours <- 
    neighbours[neighbours$row %in% 1:nrow(mat) & neighbours$col %in% 1:ncol(mat), ]
  
  return(neighbours)
}

# If an octopus has an energy level of 9, set its value to 0 and increase the
# neighbours by 1
flash_once <- function(mat) {
  # Identify all octopuses that are 0
  max_energy <- which(mat > 9, TRUE)
  
  for (i in 1:nrow(max_energy)) {
    # Set to 0
    mat[max_energy[i, 1], max_energy[i, 2]] <- 0
    
    # Get neighbours
    neighbours <- 
      get_neighbours(mat, max_energy[i, 1], max_energy[i, 2])
    
    # Increase neighbours by 1
    mat[cbind(neighbours$row, neighbours$col)] <- 
      mat[cbind(neighbours$row, neighbours$col)] + 1
  }
  
  # Set flashed octopuses back to 0
  # mat[cbind(max_energy[1], max_energy[2])] <- 0
  
  already_flashed <- mat == 0
  
  # Return matrix after the first flash, and the octopuses that already flashed.
  return(list(mat, already_flashed))
}


