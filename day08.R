
library(tidyverse)

## READ AND PREPARE DATA ##
day8 <- readLines("inputs/input08.txt")

# Separate signal patterns (10) and output (4).
day8 <- data.frame(data = day8)

day8 <- 
  day8 %>% 
  separate(data, into = c("signal", "output"), sep = "\\|") %>% 
  mutate_all(trimws) %>% 
  separate(signal, into = paste0("signal_", 1:10), sep = " ") %>% 
  separate(output, into = paste0("output_", 1:4), sep = " ")

## PART 1 ##
# In the output values, how many times do digits 1, 4, 7, or 8 appear?
# I.e. which numbers on the output use 2, 4, 3 or 7 letters?
outputs <- 
  day8 %>% 
  select(output_1:output_4)

# Count string lengths, and how many of them are == 2, 4, 3 or 7
outputs %>% 
  mutate_all(~str_length(.) %in% c(2, 4, 3, 7)) %>% 
  sum()

## PART 2 ##
# Based on the signal patterns, find a way to decode the output.
signals <- 
  day8 %>% 
  select(signal_1:signal_10)

source("day08_functions.R")

# Find numbers that correspond to the signals
signal_numbers <- vector("list", length = nrow(signals))

for (i in 1:nrow(signals)) {
  signal_numbers[[i]] <- find_signal_numbers(as.character(signals[i, ]))
}

decoded <- vector("numeric", nrow(signals))

for (i in seq_along(signal_numbers)) {
  decoded[i] <- 
    decode_output(
      as.character(outputs[i, ]), as.character(signals[i, ]), signal_numbers[[i]]
    )
}

sum(decoded)
