
day7 <- readLines("inputs/input07.txt")
day7 <- as.numeric(unlist(strsplit(day7, ",")))

## PART 1 ##
# What is the most efficient way to align the crabs horizontally?

# I don't have a clue how to solve this in a clever way, so we will just
# move the points anywhere between the min and the max of the crabs and see
# what minimises the fuel costs.
possible_values <- min(day7):max(day7)
fuel_costs <- vector("numeric", length(possible_values))

for (i in seq_along(fuel_costs)) {
  fuel_costs[i] <- sum(abs(possible_values[i] - day7))
}

min(fuel_costs)

## PART 2 ##
# Same as before, but we change the calculation of the fuel costs.

possible_values <- min(day7):max(day7)
fuel_costs <- vector("numeric", length(possible_values))

pb <- txtProgressBar(min = 0, max = length(fuel_costs), style = 3)

for (i in seq_along(fuel_costs)) {
  temp_costs <- abs(possible_values[i] - day7)
  fuel_costs[i] <- sum(
    sapply(temp_costs, function(x) {
      # Special case for input 0: cumsum(1:0) doesn't make sense
      ifelse(x == 0, 0, tail(cumsum(1:x), 1))
    })
  )
  
  setTxtProgressBar(pb, i)
}

min(fuel_costs)

# Note: I thought we can just use the mean as ideal position, because
# taking the mean (an intercept-only regression) will minimise the squared
# distance to the regression "line". At least in the example they show,
# the solution happens to be the mean.

# For the whole puzzle input, the mean is very close to the actual position,
# but we would have been off by 1.
mean(day7)
possible_values[fuel_costs == min(fuel_costs)]
