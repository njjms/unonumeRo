deal <- function(deck, nhand, nplayers){
  
  hands <- data.frame(matrix(ncol = nplayers, nrow = nhand))
  
  for (i in 1:nhand) {
    round <- deck[1:nplayers]
    hands[i ,] <- round
    
    deck <- deck[-c(1:nplayers)]
  }
  
  # for (j in 1:nplayers){
  #   hands[, j]
  # }
  return(hands)
}