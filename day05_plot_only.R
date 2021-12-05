
# Only the hydrothermal vent plot I did for day 5, without the puzzle solution

library(tidyverse)

# For blurring the lines in the plot.
# Get the package from here
# remotes::install_github("coolbutuseless/ggecho")
library(ggecho)

day5 <- read.table("inputs/input05.txt")

# Bring into dataframe with x1, y1, x2, y2 cols
day5 <- 
  day5 %>% 
  select(-V2) %>% 
  separate(V1, c("x1", "y1"), sep = ",") %>%
  separate(V3, c("x2", "y2"), sep = ",")

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
    plot.background = element_rect(fill = "#0c1017", colour = "#0c1017"),
    axis.text = element_text(size = 12, colour = "white"),
    axis.title = element_text(size = 14, colour = "white"),
    axis.line = element_line(colour = "white", size = 1),
    legend.position = "none"
  ) 
