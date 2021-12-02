
library(tidyverse)

day02 <- readLines("inputs/input02.txt")

## PART 1 ##
# Navigation of the submarine (e.g. "down 2", "forward 1").
# Multiply final horizontal position and depth.

# First: split commands ("forward", "up") from value
commands <- data.frame(full_command = day02)

commands <- 
  commands %>%
  separate(full_command, c("direction", "value"), sep = " ") %>% 
  mutate(value = as.numeric(value))

# Sum for each direction:
direction_sums <- 
  commands %>% 
  group_by(direction) %>% 
  summarise(sum_value = sum(value))

# Horizontal (forward) * depth (down - up)
direction_sums$sum_value[direction_sums$direction == "forward"] *
  (
    direction_sums$sum_value[direction_sums$direction == "down"] -
      direction_sums$sum_value[direction_sums$direction == "up"]
  )

## PART 2 ##
# "up"/"down" now adjusts the aim, and we only move (in the direction of the
# aim) when we go "forward". So this time, we'll go step by step.

# We will save the horizontal and depth values at each position so we can
# later plot them for fun.

# Calculate aim: Only change value then direction is up or down. Aim is
# cumulative.
commands <- 
  commands %>% 
  mutate(
    # Make value negative when it's up
    value = ifelse(direction == "up", -value, value),
    # Build groups: Does the command adjust the aim or not?
    adjusts_aim = ifelse(direction == "forward", FALSE, TRUE)
  ) %>% 
  group_by(adjusts_aim) %>%
  mutate(cumsum = cumsum(value))

# We need to know what the current aim is when we go forward.
# Put the aim value in a column of it's own, and fill it downwards.
# I.e. we get a new aim value when it changes.
commands <- 
  commands %>% 
  ungroup() %>% 
  mutate(aim = ifelse(direction != "forward", cumsum, NA)) %>% 
  fill(aim, .direction = "down") %>% 
  # Initial aim value is 0
  mutate(aim = ifelse(is.na(aim), 0, aim))

# Calculate depth
commands <- 
  commands %>% 
  # We only move when we go forward
  filter(direction == "forward") %>% 
  mutate(
    depth_change = value * aim,
    depth = cumsum(depth_change)
  )

# Multiply final horizontal position with final depth:
commands$cumsum[nrow(commands)] * commands$depth[nrow(commands)]

## BONUS ##
# Plot the position of the submarine - in a submarine radar-style
(
  radar_plot <- 
    commands %>% 
    select(cumsum, depth) %>% 
    mutate(command_no = row_number()) %>% 
    pivot_longer(
      cumsum:depth,
      names_to = "dimension",
      values_to = "value"
    ) %>% 
    mutate(
      dimension = ifelse(dimension == "cumsum", "horizontal", dimension),
      last_datapoint = ifelse(command_no == max(command_no), TRUE, FALSE)
    ) %>% 
    ggplot(aes(x = command_no, y = value)) +
    geom_path(colour = "#00ad2b", size = 2) +
    geom_point(
      aes(colour = last_datapoint, fill = last_datapoint, shape = last_datapoint),
      size = 5, stroke = 3
    ) +
    facet_wrap(~dimension, nrow = 2, scales = "free") +
    scale_y_continuous(expand = expansion(mult = c(0, .2))) +
    scale_colour_manual(values = c(NA, "#ff0000")) +
    scale_fill_manual(values = c(NA, "#9a2222")) +
    scale_shape_manual(values = c(NA, 21)) +
    labs(x = "horizontal") +
    theme_classic() +
    theme(
      panel.background = element_rect(fill = NA, colour = "white", size = 1),
      panel.grid.major = element_line(colour = "#00ad2b"),
      plot.background = element_rect(fill = "#0c1017", colour = NA),
      strip.background = element_rect(fill = NA),
      strip.text = element_text(size = 16, colour = "#00ad2b"),
      axis.text = element_text(size = 12, colour = "white"),
      axis.line = element_line(colour = "white", size = 1),
      legend.position = "none"
    ) 
)
