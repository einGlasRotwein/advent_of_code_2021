
library(tidyverse)

## READ IN AN FORMAT DATA ##
day5 <- read.table("inputs/input05.txt")

# Bring into dataframe with x1, y1, x2, y2 cols
day5 <- 
  day5 %>% 
  select(-V2) %>% 
  separate(V1, c("x1", "y1"), sep = ",") %>%
  separate(V3, c("x2", "y2"), sep = ",")

## PART 1 ##
# Which positions are covered by the lines?
# Only consider horizontal or vertical lines
horizontal <- 
  day5 %>% 
  filter(x1 == x2)

vertical <- 
  day5 %>% 
  filter(y1 == y2)

# Covered coordinates
covered <- 
  data.frame(
    x = vector("numeric", 0),
    y = vector("numeric", 0)
  )

# Very hacky and probably "expensive" solution, but whatever.
# Find all y-coordinates that are covered for a value of x1
for (i_row in 1:nrow(horizontal)) {
  ys <- horizontal$y1[i_row]:horizontal$y2[i_row]
  to_add <- data.frame(
    x = horizontal$x1[i_row],
    y = ys
  )
  covered <- rbind.data.frame(covered, to_add)
}

# Find all y-coordinates that are covered for a value of x1
for (i_row in 1:nrow(vertical)) {
  xs <- vertical$x1[i_row]:vertical$x2[i_row]
  to_add <- data.frame(
    y = vertical$y1[i_row],
    x = xs
  )
  covered <- rbind.data.frame(covered, to_add)
}

# How often is each position covered?
coordinate_table <- 
  covered %>% 
  unite("coordinate", x:y, sep = "-") %>% 
  group_by(coordinate) %>% 
  count()

sum(table(coordinate_table$n)[-1])
