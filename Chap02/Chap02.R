## CHAP 2: Exercises

## 2. Give R expressions that return the following
# matrices and vectors
print(cat('\nProblem 2: R matrices and vectors'))
print(cat('\n2a\n'))
a <- c(c(1:8), c(7:1))
print(a)

print(cat('\n2b\n'))
b <- c(rep(c(1:5),c(1:5)))
print(b)

print(cat('\n2c\n'))
c <- matrix(rep(1,9), nrow=3, ncol=3)
c[1,1] <- 0
c[2,2] <- 0
c[3,3] <- 0
print(c)

print(cat('\n2d\n'))
d <- matrix(c(0,2,3,0,5,0,7,0,0), nrow=3, ncol=3, byrow=TRUE)
print(d)

## Problem 3
# Suppose vec is a strictly positive vector of length 2.
# Interpreting vec as the coordinates of a point R^2, use R
# to express it in polar coordinates. You will need (at least
# one of) the inverse trigonometric functions: acos(x), asin(x),
# and atan(x)

print(cat('\nProblem3: Converting cartesian to polar coordinates.\n'))
convertCart2Polar <- function(vec, from="cart"){
  # This function converts the 2-dim vector
  # either from cartesian to polar or polar to 
  # cartesian
  
  result <- c(0,0)
  dim(result) <- c(2,1)
  if (from == "cart"){
    result[1,1] <- sqrt(vec[1,1]^2 + vec[2,1]^2)
    result[2,1] <- atan(vec[2,1]/vec[1,1])
  } else{
    result[1,1] <- vec[1,1]*cos(vec[2,1])
    result[2,1] <- vec[1,1]*sin(vec[2,1])
  }
  return(result)
}

vec = c(5, 6)
dim(vec) <- c(2,1)

print(cat('Cartesian Coordinates (x, y):', vec))
print(cat('Polar coordinates (r, theta):', convertCart2Polar(vec)))
print(cat('\n'))

## Problem 4
# Use R to produce a vector containing all integers
# from 1 to 100 that are not divisible by 2, 3, or 
print(cat('Use R to produce a vector containing all integers from 1 to 100'))
print(cat('that are not divisible by 2, 3, 7.\n'))
num = c(1:1:100)
num_result = vector()
result = numeric()

for(ind in c(1:length(num))){
  if (num[ind] %% 2 != 0 & num[ind] %% 3 != 0 & num[ind] %% 7 != 0){
    num_result <- append(num_result, num[ind], after=length(num_result))
  }
}
print(num_result)

## Problem 5
# Suppose that queue <- c("Steve", "Russel", "Alison", "Liam) and that queue
# represents a supermarket queue with Steve first in line.
# Using R expression update the supermarket queue as successively:


## Problem 7
# Build a 10x10 identity matrix. Then make all the non-zero elements 5.
# Do this latter step in at least two different ways.

print('Step 1: I = diag(5, nrow=10, ncol=10)')
print(I)
print(cat('\n'))

print('Step 2: Using for loop...')
I = diag(1, nrow=10, ncol=10)
for(ind in c(1:1:10)){
  I[ind, ind] <- 5
}
print(I)
print(cat('\n'))