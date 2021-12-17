
## READ AND PREPARE DATA -------------------------------------------------------

day15 <- readLines("inputs/input15.txt")
day15 <- as.numeric(unlist(strsplit(day15, "")))
day15 <- matrix(day15, nrow = 100)

# ## EXAMPLE
# day15 <- c(
#   "1163751742138137367221365113283694931569746341711113191281371359912421312542163912931385212311944581"
# )
# 
# day15 <- as.numeric(unlist(strsplit(day15, "")))
# 
# day15 <- matrix(day15, nrow = 10, byrow = TRUE)

## FUNCTIONS -------------------------------------------------------------------

get_neighbours <- function(mat, idx) {
  row <- idx %% nrow(mat)
  if (row == 0) row <- nrow(mat)
  col <- ceiling(idx/ncol(mat))
  
  neighbours <-
    data.frame(
      row = c(row, row, row + 1, row - 1),
      col = c(col + 1, col - 1, col, col)
    )
  
  # Exclude values outside of matrix
  neighbours <-
    neighbours[neighbours$row %in% 1:nrow(mat) & neighbours$col %in% 1:ncol(mat), ]
  
  # Turn into vector positions
  neighbours <- neighbours$row + nrow(mat) * (neighbours$col - 1)
  
  return(neighbours)
}

## PART 1 ----------------------------------------------------------------------

timer <- Sys.time()

costs <- rep(Inf, nrow(day15) * ncol(day15))
complete <- rep(FALSE, nrow(day15) * ncol(day15))

temp_node <- 1

costs[1] <- 0
complete[1] <- TRUE
temp_costs <- costs[temp_node]

next_nodes <- get_neighbours(day15, temp_node)

# Update costs
costs[next_nodes] <- 
  ifelse(
    costs[next_nodes] > temp_costs + day15[next_nodes], 
    temp_costs + day15[next_nodes], 
    costs[next_nodes]
  )

# Next node: Node with lowest costs that has not been completed yet
temp_node <- which(costs == min(costs[!complete]))[1]

while (!all(complete)) {
  temp_costs <- costs[temp_node]
  next_nodes <- get_neighbours(day15, temp_node)
  next_nodes <- next_nodes[!complete[next_nodes]]
  
  costs[next_nodes] <- 
    ifelse(
      costs[next_nodes] > temp_costs + day15[next_nodes], 
      temp_costs + day15[next_nodes], 
      costs[next_nodes]
    )
  
  complete[temp_node] <- TRUE
  if (all(complete)) break
  min_costs <- min(costs[!complete])
  temp_node <- which(costs == min_costs & !complete)
  temp_node <- temp_node[1]
  costs
}

Sys.time() - timer

costs[length(costs)]

## PART 2 ----------------------------------------------------------------------

#################################################
## HEADS UP: TAKES IN MIN TO RUN ON MY LAPTOP! ##
#################################################

# Create full map
full_map <- day15

# Extend downwards:
for (i in 1:4) {
  to_add <- day15 + i
  to_add <- to_add %% 9
  to_add[to_add == 0] <- 9
  full_map <- rbind(full_map, to_add)
}

previous_map <- full_map

# Extend to the right:
for (i in 1:4) {
  to_add <- previous_map + i
  to_add <- to_add %% 9
  to_add[to_add == 0] <- 9
  full_map <- cbind(full_map, to_add)
}

# Run algorithm
timer <- Sys.time()

costs <- rep(Inf, nrow(full_map) * ncol(full_map))
complete <- rep(FALSE, nrow(full_map) * ncol(full_map))

temp_node <- 1

costs[1] <- 0
complete[1] <- TRUE
temp_costs <- costs[temp_node]

next_nodes <- get_neighbours(full_map, temp_node)

# Update costs
costs[next_nodes] <- 
  ifelse(
    costs[next_nodes] > temp_costs + full_map[next_nodes], 
    temp_costs + full_map[next_nodes], 
    costs[next_nodes]
  )

# Next node: Node with lowest costs that has not been completed yet
temp_node <- which(costs == min(costs[!complete]))[1]

while (!all(complete)) {
  temp_costs <- costs[temp_node]
  next_nodes <- get_neighbours(full_map, temp_node)
  next_nodes <- next_nodes[!complete[next_nodes]]
  
  costs[next_nodes] <- 
    ifelse(
      costs[next_nodes] > temp_costs + full_map[next_nodes], 
      temp_costs + full_map[next_nodes], 
      costs[next_nodes]
    )
  
  complete[temp_node] <- TRUE
  if (all(complete)) break
  min_costs <- min(costs[!complete])
  temp_node <- which(costs == min_costs & !complete)
  temp_node <- temp_node[1]
  costs
}

Sys.time() - timer

costs[length(costs)]
