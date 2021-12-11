
## READ IN DATA AND FUNCTIONS --------------------------------------------------

rm(list = ls())

source("day11_functions.R")

day11 <- readLines("inputs/input11.txt")
day11 <- as.numeric(unlist(strsplit(day11, "")))
day11 <- matrix(day11, nrow = 10)

## EXAMPLE DATA
# day11 <- c(
#   "5483143223274585471152645561736141336146635738547841675246452176841721688288113448468485545283751526"
# )
# day11 <- as.numeric(unlist(strsplit(day11, "")))
# day11 <- t(matrix(day11, nrow = 10))

## PART 1 ----------------------------------------------------------------------

# Simulate octopuses for 100 steps

temp_mat <- day11

steps <- 100
n_flashes <- 0
pb <- txtProgressBar(min = 0, max = steps, style = 3)

for (i in 1:steps) {
  # Increase energy
  temp_mat <- temp_mat + 1
  # Initiate matrix with octopuses that have already flashed (same dimensions)
  # as original matrix, but filled with FALSE
  already_flashed <- temp_mat > 9
  
  while(any(temp_mat > 9)) {
    flash_out <- flash_once(temp_mat)
    temp_mat <- flash_out[[1]]
    already_flashed <- already_flashed | flash_out[[2]]
    already_flashed <- already_flashed | temp_mat > 9
  }
  
  temp_mat[already_flashed] <- 0
  n_flashes <- n_flashes + sum(already_flashed)
  
  setTxtProgressBar(pb, i)
}

n_flashes

## PART 2 ----------------------------------------------------------------------

# What is the first time all octopuses flash simultaneously?
temp_mat <- day11

step_count <- 0

while (!all(temp_mat == 0)) {
  step_count <- step_count + 1
  # Increase energy
  temp_mat <- temp_mat + 1
  # Initiate matrix with octopuses that have already flashed (same dimensions)
  # as original matrix, but filled with FALSE
  already_flashed <- temp_mat > 9
  
  while(any(temp_mat > 9)) {
    flash_out <- flash_once(temp_mat)
    temp_mat <- flash_out[[1]]
    already_flashed <- already_flashed | flash_out[[2]]
    already_flashed <- already_flashed | temp_mat > 9
  }
  
  temp_mat[already_flashed] <- 0
}

step_count
