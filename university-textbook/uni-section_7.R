# Sections 7

# Problem 7.1
unif.min = 3
unif.max = 5

# 7.1.a
# Generate 5000 random numbers with uniform distribution. Compute a histogram.
data.unif = runif(5000, unif.min, unif.max)
hist(data.unif, probability=TRUE)

# 7.1.b
# Generate 2 random numbers with uniform distribution and add them together
# Repeat this 5000 times and draw a histogram of the results

u2 = sapply(1:5000, function(x) sum(runif(2, unif.min, unif.max)))
hist(u2, probability=TRUE)

# 7.1.c
# Generate 50 random numbers with uniform distribution and add them together
# Repeat this 5000 times and draw a histogram of the results. In the same plot,
# draw a curve of a normal distribution with the following arguments:
# mean = 50 * 4, sd = sqrt(50*(4/12))

u50 = sapply(1:5000, function(x) sum(runif(50, unif.min, unif.max)))
hist(u50, probability=TRUE)
curve(dnorm(x, mean = 50 * 4, sd = sqrt(50*(4/12))),
      from = min(u50), to = max(u50), add = TRUE)

# Problems 7.2 & 7.3 are the same as 7.1, but for exponential
# and geometric distributions, respectively