# Simple R
# Section 2 - Data

# Problem 2.1
miles = c(65311, 65624, 65908, 66219, 66499, 66821, 67145, 67447)
x = diff(miles)
max(miles)
min(miles)

# Problem 2.2
commutes = c(17, 16, 20, 24, 22, 15, 21, 15, 17, 22)
# longest commute time
max(commutes)
# average commute time
mean(commutes)
# min commute time
min(commutes)
# correct the value 24 to 18
commutes[commutes==24]=18

# number of commutes longer than 20 minutes
sum(commutes >= 20)

# percentage of commutes shorter than 17 minutes
sum(commutes <= 17) / length(commutes)

# Problem 2.3
# cell phone bill by month
bill = c(46, 33, 39, 37, 46, 30, 48, 32, 49, 35, 30, 48)

#the amount you spent this year on the cell phone
sum(bill)
# the smallest amount you spent in a month
min(bill)
# the largest amount you spent in a month
max(bill)
# Number of months where the amount was greater than $40
sum(bill > 40)
# Percentage of months where the amount was greater than $40
sum(bill > 40)/length(bill)

# Problem 2.4
car.prices=c(9000, 9500, 9400, 9400, 10000, 9500, 10300, 10200)
mean(car.prices)
min(car.prices)
max(car.prices)

# Problem 2.5
# guess the results of these R commands:
x = c(1,3,5,7,9)
y = c(2,3,5,7,11,13)

x+1 # num [1:5] 2 4 6 8 10
y*2 # num [1:6] 16 6 10 14 22 26
length(x) # 5
length(y) # 6
# This expression had an unexpected result to me
# x is shorter than y, so R goes through a "recycling process"
# before adding the two vectors
# meaning, it starts repeating the elements of the vector until it reaches
# the desired length (the length of y)
# x is transformed(recycled) to [1:6] 8 3 5 7 9 1
# prior to the addition
x + y # num [1:6] 3 6 10 14 20 14
sum(x>5) # 2
sum(x[x>5]) # 16
sum(x>5 | x<3) # 3
y[3] # 5 
y[-3] # num [1:5] 2 3 7 11 13
y[x] # num [1:5] 2 5 11 NA NA
y[y>=7] # num [1:3] 7 11 13 

# Problem 2.5
x = c(1, 8, 2, 6, 3, 8, 5, 5, 5, 5)
sum(x)/length(x)
log(x, base = 10)
(x -  4.4)/2.875
diff(range(x))
max(x)-min(x)
