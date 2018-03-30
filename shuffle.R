shuffle <- function(deck) {
  tdeck <- sample(deck, length(deck), replace = FALSE)
  return(tdeck)
}