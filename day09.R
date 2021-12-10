
## EXAMPLE DATA ----------------------------------------------------------------

ex <- c("21999432103987894921985678989287678967899899965678")
ex <- as.numeric(unlist(strsplit(ex, split = "")))
ex <- matrix(ex, nrow = 5, byrow = TRUE)

## PART 1
# Find low points (points that are lower than all adjacent points)

# Adapted from this stack overflow entry:
# https://stackoverflow.com/questions/29105175/find-neighbouring-elements-of-a-matrix-in-r
# (comment by user20650)

# To get the direct neighbours, I had to change the distance to euclidean

# find distances between row and column indexes
# interested in values where the distance is one
w <-  which(ex == ex, arr.ind = TRUE)
d <- as.matrix(dist(w, "euclidean", upper = TRUE))

a <- apply(d, 1, function(i) ex[i == 1] )
names(a)  <- ex

# Find out for which list elements where number (list name) - neighbours is always
# negative
low_points <- 
  ex[
    sapply(seq_along(a), function(x) {
      temp_num <- as.numeric(names(a)[[x]])
      all((temp_num - a[[x]]) < 0)
    })
  ]

# Risk level
sum(1 + low_points)

# ## PART 2
# # Find basins around the low points
# 
# source("day09_functions.R")
# 
# # First, get locations of low points from before:
# low_point_loc <- 
#   sapply(seq_along(a), function(x) {
#     temp_num <- as.numeric(names(a)[[x]])
#     all((temp_num - a[[x]]) < 0)
#   })
# 
# low_point_loc <- 
#   which(matrix(low_point_loc, nrow = nrow(ex)), arr.ind = TRUE)
# 
# # For every low point, determine the basin
# # Go through each row (up and down) and determine how many adjacent
# # elements are not 9.
# basin_sizes <- vector("numeric", nrow(low_point_loc))
# 
# for (i_low in 1:nrow(low_point_loc)) {
#   # Start at low point
#   temp_row <- low_point_loc[i_low, 1]
#   temp_col <- low_point_loc[i_low, 2]
#   
#   # Dimensions in the same row
#   origin_row_dims <- move_along_row(ex, temp_row, temp_col)
#   temp_row_dims <- origin_row_dims[1]:origin_row_dims[2]
#   
#   basin_size <- length(temp_row_dims)
#   
#   basin_size <- basin_size + move_down(ex, temp_row, temp_row_dims)
#   basin_size <- basin_size + move_up(ex, temp_row, temp_row_dims)
#   basin_sizes[i_low] <- basin_size
# }
# 
# # Three larges basin sizes
# prod(tail(sort(basin_sizes), 3))

## PART 2, V2
# Everything that is not 9 is part of a basin

# Basin = 1, ridge = 0
basin_map <- ex
basin_map[ex < 9] <- 1
basin_map[ex == 9] <- 0

library(raster)

raster_basins <- raster(basin_map)
clumps <- as.matrix(clump(raster_basins, directions = 4))

prod(tail(sort(table(clumps)), 3))

# ACTUAL DATA ------------------------------------------------------------------

rm(list = ls())

day9 <- readLines("inputs/input09.txt")
input_length <- length(day9)
day9 <- as.numeric(unlist(strsplit(day9, split = "")))
day9 <- matrix(day9, nrow = input_length, byrow = TRUE)

## PART 1
w <-  which(day9 == day9, arr.ind = TRUE)
d <- as.matrix(dist(w, "euclidean", upper = TRUE))

a <- apply(d, 1, function(i) day9[i == 1] )
names(a)  <- day9

# Find out for which list elements where number (list name) - neighbours is always
# negative
low_points <- 
  day9[
    sapply(seq_along(a), function(x) {
      temp_num <- as.numeric(names(a)[[x]])
      all((temp_num - a[[x]]) < 0)
    })
  ]

# Risk level
sum(1 + low_points)

## PART 2
# Find basins around the low points

# Basin = 1, ridge = 0
basin_map <- day9
basin_map[day9 < 9] <- 1
basin_map[day9 == 9] <- 0

# library(raster)

raster_basins <- raster(basin_map)
clumps <- as.matrix(clump(raster_basins, directions = 4))

prod(tail(sort(table(clumps)), 3))
