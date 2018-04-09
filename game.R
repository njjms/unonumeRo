require(tibble)
require(dplyr)

ngames <- 1000
data <- matrix(, nrow=ngames)

nplayers <- 4

for (q in 1:ngames){
  
  # First we need to build the deck. The deck will be represented as a 
  # tibble with two rows: one for color (red, blue, green, yellow, wild)
  # and one for number/action
  
  turns <- 0
  
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
  
  deck <- shuffle(deck)
  
  # Let's initialize the other parts of the game
  # Let's also have a function to deal the cards
  
  players <- list()
  for (i in 1:nplayers) {
    players[[i]] = tibble(color = character(), card = character())
  }
  
  dealCards <- function(deck, players, hand=7) {
    for (j in 1:hand){
      for (i in 1:nplayers){
        players[[i]] <- rbind(players[[i]], deck[1,])
        deck <- deck[-c(1),] # remove dealt card from deck
      }
    }
    return(players)
  }
  
  removeCards <- function(deck, players, hand=7) {
    ndealt <- length(players)*hand
    deck <- deck[-c(1:ndealt),]
    return(deck)
  }
  
  players <- dealCards(deck, players)
  deck <- removeCards(deck, players)
  
  # Ok so now we have the deck and we shuffled and dealt the cards
  # Now let's play!
  
  discard <- tibble(color = character(), card = character())
  
  while (TRUE) { # flip deck until we get a number card
    top <- deck[1,] 
    deck <- deck[-c(1),]
    if (top$color != 'wild'){
      print(paste('First top card is a', top$color, top$card, sep=" "))
      break
    } else {
      discard <- rbind(top, discard)
      print('Drawing another card.')
    }
  }
  
  while (TRUE) {
    # Game will keep going until a player has 0 cards
    for (i in 1:nplayers) {
      
      if (top$color == 'wild') {
        newcolor <- sample(c('red', 'yellow', 'blue', 'green'), 1)
        top$color <- newcolor
        print(paste('Uncaught wild card. Color changed to', top$color, sep=" "))
      }
      
      if (dim(players[[i]][players[[i]]$color == top$color,])[1] > 0) {
        
        discard <- rbind(discard, top)
        top <- players[[i]][players[[i]]$color == top$color,][1,]
        players[[i]][players[[i]]$color == top$color,][1,]$color <- 'used'
        players[[i]] <- players[[i]] %>% filter(color != 'used')
        print(paste('player', i, top$color, top$card, sep = " "))
        
      } else if (dim(players[[i]][players[[i]]$card == top$card,])[1] > 0) {
        
        discard <- rbind(discard, top)
        top <- players[[i]][players[[i]]$card == top$card,][1,]
        players[[i]][players[[i]]$card == top$card,][1,]$color <- 'used'
        players[[i]] <- players[[i]] %>% filter(color != 'used')
        print(paste('player', i, top$color, top$card, sep = " "))
        
      } else if (dim(players[[i]][players[[i]]$color == 'wild',])[1] > 0) {
        
        discard <- rbind(discard, top)
        print(paste('player', i, top$color, top$card, sep = " "))
        newcolor <- sample(c('red', 'blue', 'yellow', 'green'), 1)
        top <- players[[i]][players[[i]]$color == 'wild',][1,]
        top$color <- newcolor
        print(paste('player', i, 'uses wild to change to', top$color, sep = " "))
        players[[i]][players[[i]]$card == 'wild',][1,]$color <- 'used'
        players[[i]] <- players[[i]] %>% filter(color != 'used')
        
      } else {
        
        while (TRUE) {
          
          deck <- deck[complete.cases(deck),]
          
          if (dim(deck)[1] == 1){
            
            print('Run out of cards in the deck. Have to reshuffle the deck.')
            discard <- discard[complete.cases(discard),]
            discard[discard$card == 'wild',]$color <- 'wild'
            discard[discard$card == 'draw 4',]$color <- 'wild'
            
            deck <- discard
            deck <- shuffle(deck)
            discard <- tibble(color = character(), card = character())
            
          }
          
          while (TRUE) {
            drawn <- deck[1,]
            deck <- deck[c(-1),]
            print(paste('Player', i, 'draws 1 card from the deck', sep = " "))
            
            if (is.na(drawn$color)){
              print(paste('Player', i, 'drew NA. Redrawing from deck.', sep = " "))
              discard <- rbind(discard, drawn)
              next
            }
            
            break
          }
          
          if (drawn$color == top$color || drawn$card == top$card) {
            
            discard <- rbind(top, discard)
            top <- drawn
            print(paste('player', i, 'placed', top$color, top$card, sep=" "))
            break
            
          } else if (drawn$color == 'wild') {
            
            discard <- rbind(top, discard)
            top <- drawn
            print(paste('player', i, 'placed wild ', top$card, sep=" "))
            newcolor <- sample(c('red', 'blue', 'green', 'yellow'), 1)
            top$color <- newcolor
            print(paste('player', i, 'wild card changed to', top$color, sep = " "))
            break
            
          } else {
            
            players[[i]] <- rbind(players[[i]], drawn)
            print(paste('Card drawn(',drawn$color, drawn$card, ') can\'t be played.', sep = " "))
            
          }
        }
      }
      
      if (dim(players[[i]])[1] == 1) {
        print(paste("Player", i, "has Uno!", sep = " "))
      }
      
      turns <- turns + 1
      
    }
    
    s <- sapply(players, function(x) {dim(x)[1]})
    if (any(s == 0)) {
      print(paste('Player', which(s == 0), 'has run out of cards!', sep = " "))
      break
    }

  }
  
  print(players)
  print(paste('There were', turns, 'turns', sep = " "))
  data[q] <- turns
  
}

table <- tibble(games = 1:ngames, turns = as.vector(data))
require(ggplot2)
ggplot(data = table, aes(turns)) + geom_density(color = 'black', fill = 'light green', alpha = .8) + ggtitle('1000 Simulated Uno Games')

table %>% summarise(mean = mean(turns), median = median(turns))


