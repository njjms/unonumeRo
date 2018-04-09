# Let's initialize the game. What do we need?
# We need a deck of cards, a method to shuffle, a method to deal out 
# an arbitrary M cards to N players.

source("~/Desktop/R/shuffle.R")
source("~/Desktop/R/deal.R")

nums <- rep(0:9, 2)
acts <- c(rep('reverse', 2), rep('draw2', 2), rep('skip', 2))
wilds <- c(rep('wdraw4', 4), rep('wchange', 4))

greens <- paste0('g', c(nums,acts))
blues <- paste0('b', c(nums,acts))
reds <- paste0('r', c(nums,acts))
yellows <- paste0('y', c(nums,acts))

deck <- c(greens, blues, reds, yellows, wilds)

deck <- shuffle(deck)

# To rehash the rules we are playing with, the game begins with one color card.
# You can either put a card of that color or of that number (or action).
# You can also put a wild card whenever.
# If you don't have a card to put down, you draw from the deck until you do.
# First one to run out of cards wins.


