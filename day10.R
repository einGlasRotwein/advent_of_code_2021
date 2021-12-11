
library(stringr)

## FUNCTIONS -------------------------------------------------------------------

# Define open brackets
open <- c("{", "(", "[", "<")

# Define bracket points
points <- c(3, 57, 1197, 25137)
names(points) <- c(")", "]", "}", ">")

# Check which type a bracket is
bracket_type <- function(x) {
  type <- vector("numeric", length(x))
  
  type[x %in% c("(", ")")] <- 1
  type[x %in% c("{", "}")] <- 2
  type[x %in% c("<", ">")] <- 3
  type[x %in% c("[", "]")] <- 4
  
  return(type)
}

find_illegal_bracket <- function(code) {
  code <- unlist(strsplit(code, ""))
  
  # Which brackets are open, which are closed?
  open_close <- ifelse(code %in% open, TRUE, FALSE)
  
  while (length(code) > 1) {
    # If there are no more open-close pairs, return the rest of the code
    if (all(diff(open_close) == 0)) return(paste0(code, collapse = ""))
    
    # Find the first "open-close"-pair (TRUE, FALSE; i.e. where diff is -1)
    open_idx <- which.max(diff(open_close) == -1)
    close_idx <- open_idx + 1
    
    # Check whether brackets match. If type is the same (i.e. diff == 0),
    # remove pair.
    if (diff(bracket_type(c(code[open_idx], code[close_idx]))) == 0) {
      code <- code[-c(open_idx, close_idx)]
      open_close <- ifelse(code %in% open, TRUE, FALSE)
    } else {
      # If they don't match, stop and return the illegal character
      return(code[close_idx])
    }
    
    diff(open_close)
  } 
}

reverse_string <- function(x) {
  out <- sapply(x, function(y) {
    y <- paste0(rev(unlist(strsplit(y, ""))), collapse = "")
  })
  
  out <- unname(out)
  
  return(out)
}

calculate_score_pt2 <- function(x) {
  part2_points <- 1:4
  names(part2_points) <- c("(", "[", "{", "<")
  
  x <- unlist(strsplit(x, ""))
  score <- 0
  
  for (i in 1:length(x)) {
    score <- score * 5
    score <- score + part2_points[x[i]]
  }
  
  return(score)
}

## EXAMPLE ---------------------------------------------------------------------
# Find non-matching brackets

example_input <- c(
  "[({(<(())[]>[[{[]{<()<>>", 
  "[(()[<>])]({[<{<<[]>>(",
  "{([(<{}[<>[]}>{[]{[(<()>",
  "(((({<>}<{<{<>}{[]{[]{}",
  "[[<[([]))<([[{}[[()]]]",
  "[{[{({}]{}}([{[{{{}}([]",
  "{<[[]]>}<{[{[{[]{()[[[]",
  "[<(<(<(<{}))><([]([]()",
  "<{([([[(<>()){}]>(<<{{",
  "<{([{{}}[<[[[<>{}]]]>[]]"
)

## PART 1 
illegal_bracktes <- vector("character", length(example_input))

for (i in seq_along(example_input)) {
  illegal_bracktes[i] <- find_illegal_bracket(example_input[i])
}

# Whenever it finds a single (closed) bracket, it is a corrputed line.
corrupted <- illegal_bracktes[str_length(illegal_bracktes) == 1]

# Calculate score
sum(points[corrupted])

## PART 2
# Complete incomplete lines. Our function already returned the "rest" of the
# code. We just have to invert the rest to get the closing lines.
rest <- illegal_bracktes[str_length(illegal_bracktes) > 1]
completion <- reverse_string(rest)

# Calculate score
scores <- vector("numeric", length(completion))

for (i in seq_along(completion)) {
  scores[i] <- calculate_score_pt2(completion[i])
}

# Find middle score
sort(scores)[ceiling(length(scores) / 2)]


## ACTUAL DATA -----------------------------------------------------------------
# Find non-matching bracktes

## READ IN DATA
day10 <- readLines("inputs/input10.txt")

## PART 1
illegal_bracktes <- vector("character", length(day10))

for (i in seq_along(day10)) {
  illegal_bracktes[i] <- find_illegal_bracket(day10[i])
}

# Whenever it finds an open bracket, it is a corrputed line.
corrupted <- illegal_bracktes[!illegal_bracktes %in% open]

# Calculate score
sum(points[corrupted])

## PART 2
# Complete incomplete lines. Our function already returned the "rest" of the
# code. We just have to invert the rest to get the closing lines.
rest <- illegal_bracktes[str_length(illegal_bracktes) > 1]
completion <- reverse_string(rest)

# Calculate score
scores <- vector("numeric", length(completion))

for (i in seq_along(completion)) {
  scores[i] <- calculate_score_pt2(completion[i])
}

# Find middle score
sort(scores)[ceiling(length(scores) / 2)]
