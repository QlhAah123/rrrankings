order_to_rank <- function(data) {
  # Get unique elements from data
  unique_elements <- unique(data[1,])
  # Create an empty matrix to store rankings
  rankings <- matrix(NA, nrow = nrow(data), ncol = length(unique_elements))
  # Loop over each row of data
  for (i in 1:nrow(data)) {
    # Get the order of elements in the current row
    order <- match(unique(data[1,]), data[i,])
    # Assign rankings based on the order
    rankings[i,] <- order
  }
  rankings <- data.frame(rankings)
  names(rankings) <- unique_elements
  
  return(rankings)
}