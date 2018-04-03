install.packages("tibble")
library(tibble)

# First we need to build the deck. The deck will be represented as a 
# tibble with two rows: one for color (red, blue, green, yellow, wild)
# and one for number/action

types <- c(0, rep(1:9, 2), rep('draw 2', 2), rep('skip', 2), rep('reverse', 2))

deck <- tibble(
  color = c(rep('red', 25), rep('blue', 25), rep('green', 25), rep('yellow', 25), rep('wild', 8)),
  card = c(rep(types, 4), rep('wild', 4), rep('draw 4', 4))
)

# Let's have a function to shuffle the deck

shuffle <- function(deck) {
  deck <- deck[sample(1:108, 108),]
  return(deck)
}

# Let's initialize the other parts of the game
# Let's also have a function to deal the cards

nplayers = 4

players <- list()
for (i in 1:nplayers) {
  players[[i]] = tibble(color = character(), card = character())
}

deal <- function(deck, players, hand=7) {
  for (j in 1:hand){
    for (i in length(players)){
      players[[i]] <- add_row(players[[i]], deck[1,])
      deck[-c(1),] # remove dealt card from deck
    }
  }
}
