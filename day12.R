
library(tidyverse)

source("day12_functions.R")

## day12S --------------------------------------------------------------------

# day12 <- c("start-A", "start-b", "A-c", "A-b", "b-d", "A-end", "b-end")

# day12 <- c(
#   "dc-end", "HN-start", "start-kj", "dc-start", "dc-HN", "LN-dc",
#   "HN-end", "kj-sa", "kj-HN", "kj-dc"
# )

# day12 <- c(
#   "fs-end", "he-DX", "fs-he", "start-DX", "pj-DX", "end-zg", "zg-sl", "zg-pj",
#   "pj-he", "RW-he", "fs-DX", "pj-RW", "zg-RW", "start-pj", "he-WI", "zg-he",
#   "pj-fs", "start-RW"
# )

## ACTUAL DATA -----------------------------------------------------------------

day12 <- readLines("inputs/input12.txt")

## DATA PREPARATION ------------------------------------------------------------

day12 <-
  data.frame(col = day12) %>% 
  separate(col, into = c("from", "to"))

day12_ <- day12
names(day12_) <- c("to", "from")

day12 <- rbind.data.frame(day12, day12_)

day12 <- 
  day12 %>% 
  filter(from != "end", to != "start")

## PART 1 ----------------------------------------------------------------------

current_paths <- list("start")
final_paths <- list()

while (length(current_paths) > 0) {
  current_paths <- continue_path(current_paths, day12)
  # Remove all paths that found an end.
  finish_idx <- 
    sapply(current_paths, function(x){
      x[length(x)] == "end"
    })
  
  final_paths <- c(final_paths, current_paths[finish_idx])
  current_paths <- current_paths[!finish_idx]
  
  # Remove all paths that contain a lower case letter twice
  lower_twice_idx <- 
    sapply(current_paths, function(x){
      temp_table <- table(x)
      any(temp_table[str_detect(names(temp_table), "[[:lower:]]")] > 1)
    })
  
  current_paths <- current_paths[!lower_twice_idx]
}

length(final_paths)

## PART 2 ----------------------------------------------------------------------

#####################################################################
## HEADS UP: THIS TAKES 18 MIN TO RUN ON MY REASONABLY FAST LAPTOP ##
#####################################################################

current_paths <- list("start")
final_paths <- list()

start <- Sys.time()

while (length(current_paths) > 0) {
  # Remove all paths that contain a lower case letter more than 2 times
  too_often_idx <- 
    sapply(current_paths, function(x){
      temp_table <- table(x)
      any(temp_table[str_detect(names(temp_table), "[[:lower:]]")] > 2)
    })
  
  current_paths <- current_paths[!too_often_idx]
  
  # Remove all paths that contain more than one lower case letter twice
  lower_twice_idx <- 
    sapply(current_paths, function(x){
      temp_table <- table(x)
      sum(temp_table[str_detect(names(temp_table), "[[:lower:]]")] > 1) > 1
    })
  
  current_paths <- current_paths[!lower_twice_idx]
  
  if (length(current_paths) > 0) {
    current_paths <- continue_path(current_paths, day12)
    # Remove all paths that found an end.
    finish_idx <- 
      sapply(current_paths, function(x){
        x[length(x)] == "end"
      })
    
    final_paths <- c(final_paths, current_paths[finish_idx])
    current_paths <- current_paths[!finish_idx] 
  }
  
  print(paste(length(final_paths), "paths found so far"))
}

Sys.time() - start

length(final_paths)

# beepr::beep()
