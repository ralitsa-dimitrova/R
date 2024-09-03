
# Problem 12.1

# John has 5 keys, but isn't sure which one is for his room.
# He tries each of the keys, remembering the ones he checked.
# What is the probability that he unlocks the room on the 5th key?

keys.test = function(N) {
  keys.samples = sapply(1:N, function(x) sample(1:5, 5, replace=FALSE))
  sum(keys.samples[5,] == 5)/N
}

keys.test(10000)

# Problem 12.2

# There is a deck of shuffled cards.
# The cards get distributed among 4 players.
# What is the probability that each player has one ace?

generate.deck = function() {
  suits = c("D","C","H","S")
  ranks = c(1:9,"J","Q","K","A")
  rep(ranks, times = 4)
  rep(suits, each = length(ranks))
  deck = mapply(function(x,y) paste(x, y, sep = ""),
                rep(suits, each=length(ranks)),
                rep(ranks, times = 4), SIMPLIFY= T)
  unname(deck)
}

cards.test = function(N) {
  deck = generate.deck()
  cards.samples = sapply(1:N, function (x) sample(deck, 52, replace = F))
  acesPerSample = apply(cards.samples, 2, function(x) sum(substr(x, 2,2)=="A"))
  sum(acesPerSample == ) / N
}

cards.test(999999999)

# Problem 12.6



cards.test = function(N) {
  deck = generate.deck()
  cards.samples = sapply(1:N, function (x) sample(deck, 13, replace = F))
  diamondsPerSample = apply(cards.samples, 2, function(x) sum(substr(x, 1,1)=="D"))
  heartsPerSample = apply(cards.samples, 2, function(x) sum(substr(x, 1,1)=="H"))
  sum(diamondsPerSample = 5 & heartsPerSample == 3) / N
}

cards.test(1000)

