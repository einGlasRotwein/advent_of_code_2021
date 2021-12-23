
# For part 1
count_versions <- function(hex) {
  # hex to binary
  bin <- BMS::hex2bin(hex)
  
  # Initialise
  version_counter <- 0
  # These two values determine the range we are currently
  # covering
  r1 <- 0
  r2 <- 0
  
  while (r2 != length(bin)) {
    
    # Move to version
    r1 <- r2 + 1
    r2 <- r1 + 2
    
    # End when the end is reached
    if (all(bin[r1:length(bin)] == 0)) break
    
    # Read version
    version_counter <- 
      version_counter +
      strtoi(paste0(bin[r1:r2], collapse = ""), base = 2)
    
    # Move to id
    r1 <- r2 + 1
    r2 <- r1 + 2
    
    id <- strtoi(paste0(bin[r1:r2], collapse = ""), base = 2)
    
    if (id == 4) {
      
      # Move 5 bits until end of the literal is reached
      r1 <- r2 + 1
      r2 <- r1 + 4
      
      # If the first literal is the only one, move on
      if (bin[r1] != 0) {
        
        # Else keep moving until the end is reached
        while(bin[r1] != 0) {
          r1 <- r2 + 1
          r2 <- r1 + 4
        }
        
      }
      
    } else {
      # Move to length type
      r1 <- r2 + 1
      r2 <- r1
      
      length_type <- bin[r1]
      
      # Do stuff depending on length type
      if (length_type == 1) {
        
        # Move 11 bits
        r1 <- r2 + 1
        r2 <- r1 + 10
        
      } else {
        
        # Move 15 bits
        r1 <- r2 + 1
        r2 <- r1 + 14
        
      }
    } 
    
  }
  
  return(version_counter)
}

# For part 2
# Save ID, number of sub-packages and literal values
package_summary <- function(hex) {
  # hex to binary
  bin <- BMS::hex2bin(hex)
  
  # Initialise dataframe that stores values.
  package_summary <- data.frame(matrix(nrow = 0, ncol = 5))
  names(package_summary) <- 
    c("id", "subpackages", "contains_bits", "value", "bit_length")
  
  # These two values determine the range we are currently
  # covering
  r1 <- 0
  r2 <- 0
  
  while (r2 != length(bin)) {
    
    bit_counter <- 0
    temp_package_info <- vector("numeric", 5)
    names(temp_package_info) <- 
      c("id", "subpackages", "contains_bits", "value", "bit_length")
    
    # Move to id
    r1 <- r2 + 4
    r2 <- r1 + 2
    
    bit_counter <- bit_counter + 6
    
    # End when the end is reached
    if (any(is.na(bin[r1:length(bin)]))) break
    if (all(bin[r1:length(bin)] == 0)) break
    
    temp_package_info["id"] <- strtoi(paste0(bin[r1:r2], collapse = ""), base = 2)
    
    if (temp_package_info["id"] == 4) {
      
      # Move 5 bits until end of the literal is reached
      r1 <- r2 + 1
      r2 <- r1 + 4
      literal_start <- r1
      
      bit_counter <- bit_counter + 5
      
      # If the first literal is the only one, move on
      if (bin[r1] != 0) {
        
        # Else keep moving until the end is reached
        while(bin[r1] != 0) {
          r1 <- r2 + 1
          r2 <- r1 + 4
          
          bit_counter <- bit_counter + 5
        }
        
      }
      
      # Strtoi can't handle large binaries
      temp_package_info["value"] <- 
        compositions::unbinary(paste0(bin[literal_start:r2], collapse = ""))
      
    } else {
      # Move to length type
      r1 <- r2 + 1
      r2 <- r1
      bit_counter <- bit_counter + 1
      
      length_type <- bin[r1]
      
      # Do stuff depending on length type
      if (length_type == 1) {
        
        # Move 11 bits
        r1 <- r2 + 1
        r2 <- r1 + 10
        bit_counter <- bit_counter + 11
        
        temp_package_info["subpackages"] <-
          strtoi(paste0(bin[r1:r2], collapse = ""), base = 2)
        
      } else {
        
        # Move 15 bits
        r1 <- r2 + 1
        r2 <- r1 + 14
        bit_counter <- bit_counter + 15
        
        temp_package_info["contains_bits"] <- 
          strtoi(paste0(bin[r1:r2], collapse = ""), base = 2)
        
      }
    } 
    
    temp_package_info["bit_length"] <- bit_counter
    
    package_summary <- 
      rbind.data.frame(package_summary, data.frame(as.list(temp_package_info)))
    
  }
  
  return(package_summary)
}
