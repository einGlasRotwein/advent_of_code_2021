
day1 <- as.numeric(readLines("inputs/input01.txt"))

#### PART 1 ####
# Number of times the measurement increased
sum(diff(day1) > 0)

#### PART 2 ####
# Same as above, but this time, we don't compare individual measurements, but
# use a sliding window to calculate the sum of three data points each.
sliding_sums <- zoo::rollsum(day1, 3)
sum(diff(sliding_sums) > 0)
