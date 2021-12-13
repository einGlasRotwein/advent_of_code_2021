
library(tidyverse)

## LOAD AND PREPARE DATA -------------------------------------------------------

day13 <- readLines("inputs/input13.txt")

data <- day13[!grepl("[a-z]", day13)]
data <- data[data != ""]

data <- 
  data.frame(
    input = data
  ) %>% 
  separate(
    input,
    into = c("x", "y")
  ) %>% 
  mutate_all(as.numeric)

folds <- day13[grepl("[a-z]", day13)]

folds <- 
  data.frame(
    input = folds
  ) %>% 
  separate(
    input,
    into = c("direction", "value"),
    sep = "="
  ) %>% 
  mutate(
    direction = ifelse(
      grepl("x", direction),
      "x",
      "y"
    ),
    value = as.numeric(value)
  )

## FOLDING FUNCTION

# Fold paper along a given direction.
fold <- function(x, direction, value) {
  # Only replace numbers above the folding line
  x[[direction]][x[[direction]] > value] <- 
    (value * 2) - x[[direction]][x[[direction]] > value]
  
  return(x)
}

## PART 1 ----------------------------------------------------------------------

folded <- fold(data, folds$direction[1], folds$value[1])

# How many unique data points?
folded %>% 
  unique() %>% 
  nrow()

## PART 2 ----------------------------------------------------------------------

folded <- data

for (i in 1:nrow(folds)) {
  folded <- fold(folded, folds$direction[i], folds$value[i])
  folded <- unique(folded)
}

(
  folded_plot <- 
    folded %>% 
    ggplot(aes(x = x, y = y)) +
    geom_point(shape = 15, size = 5, colour = "white") +
    scale_y_reverse(expand = c(3, 0)) +
    scale_x_continuous(expand = c(.2, 0)) +
    theme(
      panel.background = element_rect(fill = NA, colour = NA, size = 1),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "#0c1017", colour = "#0c1017"),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.line = element_blank(),
      legend.position = "none"
    ) 
)
