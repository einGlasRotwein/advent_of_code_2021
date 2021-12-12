
continue_path <- function(old_paths, nodes) {
  new_paths <- list()
  
  for (i in seq_along(old_paths)) {
    temp_node <- old_paths[[i]][[length(old_paths[[i]])]]
    temp_continue <- nodes$to[nodes$from == temp_node]
    
    # exclude visited nodes
    # temp_continue <- temp_continue[!temp_continue %in% old_paths[[i]]]
    
    temp_continue <- 
      expand.grid(
        temp_node,
        temp_continue,
        stringsAsFactors = FALSE
      )
    
    temp_continue <- split(temp_continue, seq(nrow(temp_continue)))
    temp_continue <- lapply(temp_continue, function(x) unname(unlist(x)))
    
    temp_add_paths <-
      lapply(temp_continue, function(x){
        c(old_paths[[i]], x[-1])
      })
    
    new_paths <- c(new_paths, temp_add_paths)
  }
  
  return(new_paths)
}