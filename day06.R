
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
# Obviously, this doesn't work for part 2 - I will have to use actual math
# for that part.
