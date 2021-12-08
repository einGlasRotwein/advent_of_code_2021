
# DECODING FUNCIONS #
strsplit2 <- function(x) {
  split <- unlist(strsplit(x, ""))
  return(split)
}

# Takes a signal and finds the corresponding number for each
find_signal_numbers <- function(signal) {
  
  signal_lengths <- str_length(signal)
  signal_numbers <- rep(999, length(signal)) # fill with solutions
  
  # Identify unique numbers
  # Unique numbers
  signal_numbers[signal_lengths == 2] <- 1
  signal_numbers[signal_lengths == 3] <- 7
  signal_numbers[signal_lengths == 4] <- 4
  signal_numbers[signal_lengths == 7] <- 8
  
  ## FIND 6 ##
  # A signal with length 6 that shares only one segment with number 1 must be
  # number 6
  signal_numbers[signal_lengths == 6][
    sapply(
      strsplit(signal[signal_lengths == 6], split = ""), function(x) {
        length(
          setdiff(strsplit2(signal[signal_numbers == 1]), x)
        ) == 1
      }
    )
  ] <- 6
  
  ## FIND 5 ##
  # One segment less than 6 must be 5
  signal_numbers[signal_lengths == 5][
    sapply(
      strsplit(signal[signal_lengths == 5], split = ""),
      function(x) {
        length(
          setdiff(strsplit2(signal[signal_numbers == 6]), x)
        ) == 1
      }
    )
  ] <- 5
  
  ## FIND 3 AND 9 ##
  # 3 and 9 are exactly the same, but 9 has one segment more
  temp_split <- strsplit(signal[signal_numbers == 999], split = "")
  combs <- combn(4, 2)
  
  for (i in 1:ncol(combs)) {
    if(length(temp_split[[combs[1, i]]]) == length(temp_split[[combs[2, i]]])) next
    
    if (
      length(intersect(temp_split[[combs[1, i]]], temp_split[[combs[2, i]]])) == 5
    ) {
      
      # 3 first, then 9
      if (length(temp_split[[combs[1, i]]]) == 5) {
        result <- c(combs[1, i], combs[2, i])
        break
      } else {
        result <- c(combs[2, i], combs[1, i])
        break
      }
      
    }
  }
  
  signal_numbers[signal_numbers == 999][result] <- c(3, 9)
  
  ## FIND 0 AND 2 ##
  # Now, the one left with 6 letters is 0, and the other is 2
  signal_numbers[signal_numbers == 999 & signal_lengths == 6] <- 0
  signal_numbers[signal_numbers == 999] <- 2
  
  return(signal_numbers)
}

decode_output <- function(output, signal, signal_numbers) {
  decoded <- rep(NA, length(output))
  
  for (i in 1:length(strsplit(output, split = ""))) {
    temp_code <- strsplit(output, split = "")[[i]]
    
    decoded[i] <- 
      signal_numbers[
        sapply(strsplit(signal, split = ""), function(x){
          # length(setdiff(x, temp_code)) == 0
          all(x %in% temp_code) & all(temp_code %in% x)
        })
      ]
  }
  
  decoded <- as.numeric(paste0(decoded, collapse = ""))
  
  return(decoded)
}

