
## DATA AND FUNCTIONS ----------------------------------------------------------

source("day16_functions.R")

day16 <- readLines("inputs/input16.txt")

## PART 1 ----------------------------------------------------------------------

count_versions(day16)

## PART 2 ----------------------------------------------------------------------

# hex <- "9C0141080250320F1802104A08" 
pk_summary <- package_summary(day16)
pk_summary
