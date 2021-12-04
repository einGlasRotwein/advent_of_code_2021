
library(tidyverse)

# Read in data
rand_numbers <- readLines("inputs/input04.txt", 1)
rand_numbers <- as.numeric(unlist(strsplit(rand_numbers, ",")))

bingo_boards <- read_lines("inputs/input04.txt", skip = 2)

# For the bingo boards: They are divided by "". Put each pack of 5 lines
# in a list. I bet there's less ugly solutions for this.
bingo_list <- list()

# 6 lines per board
for (i in 1:(length(bingo_boards)/6)) {
  first <- 1 + ((i - 1) * 6)
  last <- 6 + ((i - 1) * 6) - 1 # skip empty row
  bingo_list[[i]] <- bingo_boards[first:last]
}

# Clean up
bingo_list <- lapply(bingo_list, trimws)

# Turn vectors into data frame
bingo_list <- 
  lapply(bingo_list, function(x) {
    trimws(x)
    
    data.frame(col1 = x) %>% 
      separate(col1, into = paste0("col", 1:5))
  })

## PART 1 ##
# Play bingo.

# Create a "mirrored" list with TRUE/FALSE where we collect the "hits".
# This is a quick and dirty way to create such a list - we know it doesn't
# contain negative numbers, so if I ask whether the whole thing is -1, I will
# get a list of data frames where all cells are FALSE.
bingo_hits <- lapply(bingo_list, `<`, -1)

# Draw numbers and mark them on the hit board
for (i_draw in rand_numbers) {
  temp_hits <- lapply(bingo_list, `==`, i_draw)
  
  # Update hit list
  bingo_hits <- Map(`|`, temp_hits, bingo_hits)
  
  # Do we have a bingo somewhere?
  rowsums <- lapply(bingo_hits, rowSums)
  colsums <- lapply(bingo_hits, colSums)
  
  # If so, stop
  if (any(unlist(lapply(rowsums, `==`, 5)))) break
  if (any(unlist(lapply(colsums, `==`, 5)))) break
}

# Where is the bingo?
any(unlist(lapply(rowsums, `==`, 5)))
any(unlist(lapply(colsums, `==`, 5))) # in the columns

winner <- which(unlist(lapply(lapply(colsums, `==`, 5), any)))

# Calculate board score.
# Number of all unmarked numbers on this board:
unmarked <- 
  bingo_list[[winner]][
    !bingo_hits[[winner]]
  ]

# Multiply with latest draw
sum(as.numeric(unmarked)) * i_draw

## PART 2 ##
# Which board wins last?

# Modify function above. Iteratively remove the bords that won.
bingo_hits <- lapply(bingo_list, `<`, -1) # reset hit counter
temp_boards <- bingo_list # keep bingo list intact

# Draw numbers and mark them on the hit board
for (i_draw in rand_numbers) {
  temp_hits <- lapply(temp_boards, `==`, i_draw)
  
  # Update hit list
  bingo_hits <- Map(`|`, temp_hits, bingo_hits)
  
  # Do we have a bingo somewhere?
  rowsums <- lapply(bingo_hits, rowSums)
  colsums <- lapply(bingo_hits, colSums)
  
  # If so, remove this board
  row_wins <- unlist(lapply(rowsums, function(x) any(x == 5)))
  col_wins <- unlist(lapply(colsums, function(x) any(x == 5)))
  
  remove_boards <- row_wins | col_wins
  
  # Only remove boards when there is more than 1 left
  if (length(temp_boards) != 1) {
    temp_boards <- temp_boards[!remove_boards] 
    bingo_hits <- bingo_hits[!remove_boards]
  } else {
    # Stop when the last board has won
    if(any(remove_boards)) break
  }
}

# Calculate the score of the remaining board (after it won)
unmarked <- 
  temp_boards[[1]][
    !bingo_hits[[1]]
  ]

sum(as.numeric(unmarked)) * i_draw
