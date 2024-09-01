
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

