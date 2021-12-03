
library(tidyverse)

day3 <- readLines("inputs/input03.txt")

## PART 1 ##
# Find most/least frequent bit in each position.

day3_df <- data.frame(input = day3)

# Split bits
day3_df <- 
  day3_df %>% 
  # Seperate leaves the first column empty here, for some reason, so we'll
  # need to use 13 columns and discard the first one
  separate(input, into = paste0("bit_", 0:12), sep = "") %>% 
  select(-bit_0)

# There is no mode function in base R
stat_mode <- function(x) {
  result <- as.numeric(names(table(x)[table(x) == max(table(x))]))
  return(result)
}

# Which is the most common one?
gamma <- 
  day3_df %>% 
  summarise_all(~stat_mode(.)) %>% 
  slice(1) %>% 
  unlist(., use.names = FALSE)

# Least common one
epsilon <- as.numeric(!gamma)

# Collapse into single string
gamma <- paste0(gamma, collapse = "")
epsilon <- paste0(epsilon, collapse = "")

# Get decimal
strtoi(gamma, base = 2) * strtoi(epsilon, base = 2)

## PART 2 ##
# Iterative process: Keep the numbers with the most common first bit.
# Then keep the numbers with the most common 2nd bit etc.
# Stop when a single number is left

temp_df <- day3_df

for (i_col in 1:length(day3_df)) {
  filter_bit <- stat_mode(temp_df[[i_col]])
  # Filter for bit 1 when 0 and 1 are equally common
  if(length(filter_bit) > 1) filter_bit <- 1
  
  temp_df <- temp_df[temp_df[[i_col]] == filter_bit, ]
  if(nrow(temp_df) == 1) break
}

oxygen <- paste0(unlist(temp_df, use.names = FALSE), collapse = "")

# Now: Same, but with the least common bit
temp_df <- day3_df

for (i_col in 1:length(day3_df)) {
  filter_bit <- stat_mode(temp_df[[i_col]])
  # reverse (we want the least common)
  filter_bit <- as.numeric(!filter_bit)
  # Filter for bit 0 when 0 and 1 are equally common
  if(length(filter_bit) > 1) filter_bit <- 0
  
  temp_df <- temp_df[temp_df[[i_col]] == filter_bit, ]
  if(nrow(temp_df) == 1) break
}

co2 <- paste0(unlist(temp_df, use.names = FALSE), collapse = "")

# Get decimal
strtoi(oxygen, base = 2) * strtoi(co2, base = 2)
