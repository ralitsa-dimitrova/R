# Section 8
# Hypothesis testing with one sample

# Section 8.1 - 1-sample z-test for proportion

# Example

# Ivan flipped a coin 100 times, and it landed on heads 64 times.
# Is there reason to claim that the coin is biased, i.e., that the
# probability of landing on heads is greater than 1/2?

n=100; x=64; p0=0.5
prop.test(x = x, n = n, p = p0, alternative = "greater", correct = F)
z.obs = ((x / n) - p0)/sqrt(p0 * (1 - p0) / n)
p.value = 1 - pnorm(z.obs) # [1] 0.00255513
# The P-value is too low, therefore we reject the hypothesis
# that p=0.5 (i.e. the coin is biased)

# Problem 8.1

# It is known that 10% of cars of a given brand develop a serious defect during
# the warranty period. Out of the first 25,000 cars sold from a new model,
# 2,700 were found to have a defect. Can it be claimed that the percentage of
# defective cars of this model will be different from 10%?

n=25000; x=2700; p0=0.1
prop.test(x = x, n = n, p = p0, correct = F)
z.obs = ((x / n) - p0)/sqrt(p0 * (1 - p0) / n)
p.value = 2*abs(1 - pnorm(z.obs))

# Problem 8.2

# A drug is effective in 72% of cases when treating an infection. A new drug has
# been developed and tested on 50 patients, and it was effective in 42 of them.
# Do we have sufficient grounds to claim that the new drug will be effective in
# more than 72% of cases?

n=50; x=42; p0=0.72
prop.test(x = x, n = n, p = p0, alt="greater", correct = F)
z.obs = ((x / n) - p0)/sqrt(p0 * (1 - p0) / n)
p.value = 1 - pnorm(z.obs)

# Section 8.2 - 1-sample z-test for mean

# Problem 8.3

# A company manufactures light bulbs. The average lifespan of a bulb is
# 2,000 hours with a standard deviation of 300 hours. A new type of bulb has
# been proposed. A sample of 100 bulbs of the new type was tested. The results
# showed an average lifespan of 2,100 hours and the same standard deviation.
# Can it be claimed that the average lifespan of the new type of bulbs is
# greater than 2,000 hours?

x.mean = 2100; var = 300; n = 100; mean.0 = 2000
z.obs = (x.mean - mean.0)/(var/sqrt(n))
p.value = 1-pnorm(z.obs) # [1] 0.0004290603 < 0.05 => H0 is rejected

# Problem 8.4

# According to historical data, the average acidity of rain in a certain
# industrial area is 5.2. To check if there has been any change in this value,
# the acidity of 12 rainfalls over the past year was measured. The following
# results were obtained:
#
# 6.1 5.4 4.8 5.8 6.6 5.3 6.1 4.4 3.9 6.8 6.5 6.3
#
# Do we have grounds to claim that the acidity in the area has changed compared
# to the historical data?

rain.data = c(6.1, 5.4, 4.8, 5.8, 6.6, 5.3, 6.1, 4.4, 3.9, 6.8, 6.5, 6.3)
qqnorm(rain.data); qqline(rain.data)
n = length(rain.data); mu = 5.2
t.obs = (mean(rain.data) - mu) / (sd(rain.data)/sqrt(n))
p.value = 2*(1-abs(pt(t.obs, n - 1)))
p.value <= 0.05
t.test(x = rain.data, mu = mu)
