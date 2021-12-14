
library(tidyverse)

## READ AND PREPARE DATA -------------------------------------------------------

day14 <- readLines("inputs/input14.txt")

start <- day14[1]
start <- unlist(strsplit(start, ""))

day14 <- day14[-(1:2)]

day14 <-
  data.frame(input = day14) %>%
  separate(
    input,
    into = c("pair", "add"),
    sep = " -> "
  ) %>%
  separate(
    pair,
    into = c("pair1", "pair2"),
    sep = "(?<=[A-Z])",
    remove = FALSE
  )

## EXAMPLE
# start <- "NNCB"
# start <- unlist(strsplit(start, ""))
# 
# day14 <- c(
#   "CH -> B","HH -> N", "CB -> H","NH -> C","HB -> C","HC -> B", "HN -> C",
#   "NN -> C","BH -> H","NC -> B", "NB -> B","BN -> B","BB -> N","BC -> B",
#   "CC -> N","CN -> C"
# )
# 
# day14 <-
#   data.frame(input = day14) %>%
#   separate(
#     input,
#     into = c("pair", "add"),
#     sep = " -> "
#   ) %>%
#   separate(
#     pair,
#     into = c("pair1", "pair2"),
#     sep = "(?<=[A-Z])",
#     remove = FALSE
#   )


## FUNCTION --------------------------------------------------------------------

build_pairs <- function(start) {
  pair_idx <- 
    data.frame(
      no1 = 1:(length(start) - 1),
      no2 = 2:length(start)
    )
  
  pairs <- 
    apply(pair_idx, 1, function(x) {
      paste0(c(start[x[1]], start[x[2]]), collapse = "")
    })
  
  return(pairs)
}

extend_polymers <- function(start, polymer_table) {
  pairs <- build_pairs(start)
  
  to_add <- polymer_table$add[match(pairs, polymer_table$pair)]
  
  new_polymer <- 
    c(c(rbind(start[-length(start)], to_add)), start[length(start)])
  
  return(new_polymer)
}

## PART 1 ----------------------------------------------------------------------

steps <- 10
temp_start <- start

for (i in 1:steps) {
  temp_start <- extend_polymers(temp_start, day14)
}

max(table(temp_start)) - min(table(temp_start))

## PART 2 ----------------------------------------------------------------------

timer <- Sys.time()

steps <- 40
temp_start <- start

# Break down to unique pairs
all_pairs <- build_pairs(temp_start)
pairs_table <- table(all_pairs)

letter_sums <- table(temp_start)

pb <- txtProgressBar(min = 0, max = steps, style = 3)

for (i in 1:steps) {
  
  new_polymers <- 
    lapply(names(pairs_table), function(x) {
      x <- unlist(strsplit(x, ""))
      extend_polymers(x, day14)
    })
  
  # Middle letter is new and added to the count
  add_to_count <- pairs_table
  
  names(add_to_count) <- 
    sapply(new_polymers, function(x) {
      x[2]
    })
  
  letter_sums <- c(letter_sums, add_to_count)
  letter_sums <- by(unlist(letter_sums), names(unlist(letter_sums)), sum)
  sum_names <- names(letter_sums)
  letter_sums <- as.vector(letter_sums)
  names(letter_sums) <- sum_names
  
  all_pairs <- lapply(new_polymers, build_pairs)
  all_pairs <- lapply(all_pairs, table)
  all_pairs <- Map("*", all_pairs, pairs_table)
  all_pairs <- by(unlist(all_pairs), names(unlist(all_pairs)), sum)
  
  pairs_table <- as.vector(all_pairs)
  names(pairs_table) <- names(all_pairs)
  
  setTxtProgressBar(pb, i)
}

Sys.time() - timer

options(scipen = 999)
max(letter_sums) - min(letter_sums)
