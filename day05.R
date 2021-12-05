
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

## PART 2 ##
# Also consider diagonal lines (which are luckily 45 degrees)
diagonal <- 
  day5 %>% 
  filter(x1 != x2 & y1 != y2)

for (i_row in 1:nrow(diagonal)) {
  xs <- diagonal$x1[i_row]:diagonal$x2[i_row]
  ys <- diagonal$y1[i_row]:diagonal$y2[i_row]
  
  to_add <- data.frame(
    y = ys,
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

## BONUS ##
# Get the package from here
# remotes::install_github("coolbutuseless/ggecho")
library(ggecho)

# Plot danger zones
to_plot <- 
  day5 %>% 
  rownames_to_column("id") %>% 
  pivot_longer(
    x1:y2,
    names_to = c("axis", "position"),
    values_to = "value",
    names_pattern = "(.)(.)"
  ) %>% 
  pivot_wider(names_from = "axis") %>% 
  mutate(across(x:y, as.numeric))

vent_plot <- 
  to_plot %>% 
  ggplot(aes(x = x, y = y, group = id)) +
  geom_path(colour = "#00f0d5", alpha = .5, stat = "echo", alpha_factor = .3) +
  geom_point(colour = "#00f0d5") +
  theme_classic() +
  
  theme(
    panel.background = element_rect(fill = NA, colour = NA, size = 1),
    panel.grid.major = element_line(colour = "#005a5f"),
    plot.background = element_rect(fill = "#00152e", colour = "#00152e"),
    axis.text = element_text(size = 12, colour = "white"),
    axis.title = element_text(size = 14, colour = "white"),
    axis.line = element_line(colour = "white", size = 1),
    legend.position = "none"
  ) 

