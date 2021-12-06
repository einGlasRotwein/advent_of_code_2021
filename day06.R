
day6 <- readLines("inputs/input06.txt")
day6 <- as.numeric(unlist(strsplit(day6, ",")))

## PART 1 ##
days <- 80
lanternfish <- day6

for (i in 1:days) {
  # How many lanternfish to add?
  to_add <- sum(lanternfish == 0)
  
  # For each day, decrease lanternfish counter.
  lanternfish <- lanternfish - 1
  
  if (to_add != 0) {
    # Reset counters
    lanternfish[lanternfish == -1] <- 6
    # Add new fish
    lanternfish <- c(lanternfish, rep(8, to_add)) 
  }
}

# Number of fish after 80 days
length(lanternfish)

## PART 2 ##
# Obviously, this doesn't work for part 2 - it's computationally too expensive.

# Instead, we count how many fish we have for each status
table(day6)

# We create a vector of possible states (0 - 8) and then move the number of
# fish from position to position.
fish_per_state <- rep(0, 9)
names(fish_per_state) <- 0:8

# Transfer fish that we initially have
fish_per_state[names(fish_per_state) %in% names(table(day6))] <- table(day6)
# fish_per_state[names(fish_per_state) %in% names(table(c(3,4,3,1,2)))] <- 
#   table(c(3,4,3,1,2))

days <- 256

for (i in 1:days) {
  # All fish with the state 0 spawn new fish.
  # They are added to the fish with state 8, but only start counting down
  # next round
  new_spawn <- fish_per_state[names(fish_per_state) == 0]
  
  # Next, the fish with states 1 - 7 move one state down, while the fish
  # which status 0 move to status 6.
  fish_per_state <- fish_per_state[c(2:7, 1, 8:9)]
  # reset names
  names(fish_per_state) <- 0:8
  
  # The fish from 7 are added to 6
  fish_per_state[names(fish_per_state) == 6] <- 
    fish_per_state[names(fish_per_state) == 6] + 
    fish_per_state[names(fish_per_state) == 7]
  
  # The fish from 8 move to 7 (the fish in 7 are just moved to the end
  # as a dummy)
  fish_per_state <- fish_per_state[c(1:7, 9, 8)]
  names(fish_per_state) <- 0:8
  
  # Add newly spawned fish
  fish_per_state[names(fish_per_state) == 8] <- new_spawn
}

options(scipen = 999)
sum(fish_per_state)
