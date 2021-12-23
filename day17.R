
## PART 1 ----------------------------------------------------------------------

# Initial value to fire the probe as high as possible, while still reaching
# the target area

# Initial velocity.
# Move until target area is reached (or out of reach)
move_probe <- function(x, y, xmin, xmax, ymin, ymax) {
  xpos <- 0
  ypos <- 0
  highest_y <- 0
  
  # Stop when position is reached
  while (!xpos %in% xmin:xmax | !ypos %in% ymin:ymax) {
    xpos <- xpos + x
    ypos <- ypos + y
    
    if (x > 0) x <- x - 1
    if (x < 0) x <- x + 1
    y <- y - 1
    
    # Update highest y
    highest_y <- ifelse(ypos > highest_y, ypos, highest_y)
    
    # End if it's hopeless
    if (xpos > xmax | ypos < ymin) return(NA)
  }
  
  return(highest_y)
}

# Example target area: x=20..30, y=-10..-5
xmin <- 20
xmax <- 30
ymin <- -10
ymax <- -5

move_probe(6, 9, xmin, xmax, ymin, ymax)

# target area: x=282..314, y=-80..-45
xmin <- 282
xmax <- 314
ymin <- -80
ymax <- -45

# Go through possible values of x and determine the highest value
# for possible y-values

positions <- data.frame(x = 1:xmax, highest_y = NA, y = NA)

# Optimise y-values for each x value we found
for (i in 1:nrow(positions)) {
  x <- positions$x[i]
  highest_y <- NA
  y <- 1
  y_cap <- 100
  
  while(y < y_cap) {
    highest_y <- c(highest_y, move_probe(x, y, xmin, xmax, ymin, ymax))
    y <- y + 1
  }
  
  if (!all(is.na(highest_y))) {
    positions$highest_y[i] <- max(highest_y, na.rm = TRUE)
    positions$y[i] <- which(highest_y == max(highest_y, na.rm = TRUE)) - 1
  }
}

positions[!is.na(positions$highest_y), ]
max(positions$highest_y, na.rm = TRUE)

## PART 2 ----------------------------------------------------------------------

# How many initial values meet the target?

positions <- data.frame(x = 1:xmax, sum_trajectories = NA)

# Optimise y-values for each x value we found
for (i in 1:nrow(positions)) {
  x <- positions$x[i]
  y <- -100
  highest_y <- NA
  y_cap <- 100
  
  # TO DO: CHANGE! DON'T STOP AT FIRST VALID VALUE, BUT COUNT VALID VALUES!
  # Move until a valid position is found or y_cap is reached
  while(y < y_cap) {
    highest_y <- c(highest_y, move_probe(x, y, xmin, xmax, ymin, ymax))
    y <- y + 1
  }
  
  if (!all(is.na(highest_y))) {
    positions$sum_trajectories[i] <- length(highest_y[!is.na(highest_y)])
  }
}

sum(positions[!is.na(positions$sum_trajectories), ]$sum_trajectories)
