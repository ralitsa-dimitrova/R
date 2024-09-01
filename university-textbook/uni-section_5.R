# Problems from university notes

N = 1000 # Total number of samples

# Problem 5.1

# There is box with 5 balls inside it, numbered 1 through 5.
# You retrieve a ball from the box, return it and then retrieve a ball again.
# What is the probability to retrieve the same ball twice?

# Solution

balls.test = function(N) {
  ball.samples = sapply(1:N, function (x) sample(1:5, 2, replace=TRUE))
  sum(ball.samples[1,] == ball.samples[2,])/N
}

# Run

balls.test(1000)
balls.test(10000)
balls.test(100000)

# Problem 5.2

# Throwing a dice. 
# Is the relative frequency of throwing a 6 close to the theoretical probability of 1/6?

# Solution

dice.test = function(N) {
  dice.samples = sample(1:6, N, replace = TRUE)
  table(dice.samples)[6]/N
}

dice.test(10000)

# Problem 5.3
# What is the probability that out of a group of 20 people, 
# there will be at least two people who share a birthday?

bdays.data = sample(1:365, 20, T) # Generate 1 sample
N = 100 # Total samples
bdays.table = sapply(1:N, function (x) sample(1:365, 20, T)) # Generate N samples
table(bdays.table[,1]) # Group b-day dates by people
max(table(bdays.table[,1])) # Get the count of the people which have a b-day on the most common b-day date
bdays.mostcommondates = apply(bdays.table, 2, function(x) max(table(x)))
sum(bdays.mostcommondates>=2)

bday.problem = function(N) {
  bday.samples = sapply(1:N, function (x) sample(1:365, 20, T))
  bday.peopleByDateCount = apply(bday.samples, 2, function(x) max(table(x)))
  sum(bday.peopleByDateCount>=2)/N
}

bday.problem(10000)

# Problem 5.4
# In the course of a week you receive a total of 12 phone calls.
# What is the probability that you will get at least one phone call on each
# day of the week?


calls = sample(1:7, 12, T) # Generate 1 sample
calls.table = sapply(1:N, function(x) sample(1:7, 12, T), T)




countWeekdaysWithCalls = function(calls.sample) {
  b = vector("numeric", 7)
  for (i in 1:length(calls.sample)) {
    b[calls.sample[i]] = 1
  }
  sum(b)
}

# Solution A

calls = function(N) {
  calls.samples = sapply(1:N, function(X) sample(1:7, 12, replace = TRUE))
  calls.weekdays = apply(calls.samples, 2, function(x) countWeekdaysWithCalls(x))
  sum(calls.weekdays==7)/N
}

calls(1000)


# Solution B - With a for cycle

calls.cycleImpl = function(N) {
  r = 0
  for (i in 1:N) {
    call.sample = sample(1:7, 12, replace=TRUE)
    weekdaysWithCalls = countWeekdaysWithCalls(call.sample)
    if (weekdaysWithCalls == 7) r = r+1
  }
  r/N
}

calls.cycleImpl(1000)

# Problem 5.5

# There are 3 playing cards
# - 1) Both sides are white
# - 2) Both sides are black
# - 3) One side is white, the other - black
# We choose a random card. The side which is facing up is white.
# What is the probability that the other side is also white?

gen.sample = function() {
  card = sample(c("bb", "bw", "ww"), 1)
  side = sample(1:2, 1)
  up = substr(card, side, side)
  c(card, up)
}

cards.test = function(N) {
  samples = sapply(1:N, function(x) gen.sample())
  sum(samples[1,] == "ww") / sum(samples[2,] == "w")
}
