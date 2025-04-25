#---------------------------------------------------------
# File:   mit18_05_s22_studio4-samplecode.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
#Sample code for 18.05 Studio 4
#--------------------------
# Generating random numbers from distributions

# rnorm(n, mu, sigma) gives us n random values 
# from Norm(mu, sigma^2)

# Examples
# 1. Simulate 5 trials with a standard  
# normal variable, i.e. mu=0, sigma=1 
x = rnorm(5, 0, 1)
print(x)

# 2. Run 5 trials with mu=100, sigma=15 
#   (so sigma^2 = 225)
x = rnorm(5, 100, 15)  
print(x)

# 3. Test the 0.68, 0.95 rules
x = rnorm(10000, 0, 1)
m1 = mean(abs(x) < 1)
m2 = mean(abs(x) < 2)
cat('Mean m1 should be near 0.68: m1 = ', m1, '\n')
cat('Mean m2 should be near 0.95: m2 = ', m2, '\n')

# Likewise for the exponential, uniform and
# many other distributions
# 4. 5 trials with an exponential(2.5),
#    5 trials with a uniform(0, 1) 
#    5 trials with a uniform variable on 
#    the interval [0, 100]
x = rexp(5, 2.5)
print(x)
x = runif(5, 0, 1)
print(x)
x = runif(5, 0, 100)
print(x)

#------------------------------
# In R, every distribution has functions
# r*, d*, p*, q*

# Let's look at rnorm, dnorm, pnorm, qnorm

# We have already done rnorm -- see above

# dnorm is the pdf: it gives the values of the 
# density at x

# Here is the standard normal density evaluated at 
# x = 0
x = 0
d = dnorm(x, 0, 1)
print(d)

# x can also be a list of values. 
# Here  dnorm returns a list of density values.
x = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)
d1 = dnorm(x, 0, 1)
print(d1)

d2 = dnorm(x, 0, 2)
print(d2)

# We can use dnorm to plot the standard 
# normal density

#Make sequence from -6 to 6 in steps of 0.01
z = seq(-6, 6, 0.01)  
# Evaluate dnorm at every point in z
y = dnorm(z, 0, 1)
# Plot the result
plot(z, y, type='l', col='blue', lwd=2)

# We can add some other normal densities in 
# other colors
lines(z, dnorm(z, 1, 1), col='orange', lwd=2)
lines(z, dnorm(z, 0, 2), col='green', lwd=2)

#------------------------------
# pnorm is the cdf: pnorm(x) gives the cdf evaluated
# at x. That is, the probability a random normal
# variable is less than or equal to x

# The standard normal cdf evaluated at 0 should
# give 0.5
x = 0
x = pnorm(x, 0, 1)
print(x)

# Just like dnorm, we can evaluate pnorm on a
# list of values
x = c(0, 0.2, 0.4, 0.6, 0.8, 1.0)
p1 = pnorm(x, 2, 4)
print(p1)

# We can use pnorm to plot the standard normal cdf
z = seq(-6, 6, 0.01)  
y = pnorm(z, 0, 1)
plot(z, y, type='l', col='blue', lwd=3)
# Add some other cdf's
lines(z, pnorm(z, 1, 1), col='orange', lwd=3)
lines(z, pnorm(z, 0, 2), col='green', lwd=3)

#------------------------------
### QNORM is not used in studio 4.

# qnorm finds quantiles. 
# It is the inverse of the cdf it takes a 
# probability and produces a value. That is,
#a = qnorm(p, 0, 1)  <==> P(Z<a) = p 
#                    <==> p = pnorm(a, 0, 1)
# Here is the inverse in action
p = 0.5 # a probability
q = qnorm(p, 0, 1) # The x value that gives prob. p
pcheck = pnorm(q, 0, 1) # give back the original p
cat("p = ", p, "; q = ", q, "; pcheck = ", pcheck, "\n")

#------------------------------
#Histograms
# R makes plotting histograms of data easy

# Generate some random exponential data and plot a histogram
data = rexp(100, 0.5)
# Let R make the choice of the number of bins
# freq = FALSE makes a density histogram
hist(data, col='orange', freq=FALSE)

# Controlling the number of bins: hist has
# a named argument called breaks.

# If breaks is a single number, then
# hist uses it as the number of bins.
# (Actually this just 'suggests' the number of 
# bins. I'm not sure how R chooses the exact
# number)
data = rexp(100, 0.5)
# Run these one at a time in RStudio
hist(data, breaks=12, col='blue', freq=FALSE)
hist(data, breaks=100, col='orange', freq=FALSE)
hist(data, breaks=2, col='cyan', freq=FALSE)

# Breaks can be a list which gives the endpoints of 
# each bin
lambda = 0.5
data = rexp(10000, lambda)

# Make bins of width 0.4 that start at the minimum
# data value and go to the maximum data
binwidth = 0.3
bins = seq(min(data), max(data)+binwidth, binwidth)
hist(data, breaks=bins, col='orange', freq=FALSE)

# Add a plot of the pdf 
x = seq(0, max(data)+1, 0.01)
lines(x, dexp(x, lambda), col='blue', lwd=2)

#------------------------------
# var(), cov(), cor()
# These functions compute the sample variance, 
# covariance and correlation of sampled data.

# First we generate 3 independent sets of 
# random data
x1 = rnorm(100, 2, 5)  # 100 samples from N(2, 5^2)
x2 = rnorm(100, 2, 5)
x3 = rnorm(100, 2, 5)
cat('var(x1) =', var(x1), '\n')
cat('var(x2) =', var(x2), '\n')
cat('var(x3) =', var(x3), '\n')

# We combine these in overlapping sums to have 
# samples from non-independent variables
x = x1 + x2
y = x2 + x3

# Now we can compute sample means, variance, covariance, correlation
xMean = mean(x)
yMean = mean(y)
xVar = var(x)
yVar = var(y)
xyCov = cov(x, y)
xyCor = cor(x, y)

# Here's another of the many ways to print something
print(paste("xMean = ", xMean))
print(paste("yMean = ", yMean))
print(paste("xVar = ", xVar))
print(paste("yVar = ", yVar))
print(paste("xyCov = ", xyCov))
print(paste("xyCor = ", xyCor))

#--------------------------------------
# Random sampling from an arbitrary
# discrete distribution

# You can use the sample function to
# randomly sample from any given distribution.

# We make up an arbitrary distribution
values = c(-1, 3, 7, 11)   # 4 possible value
probs = c(0.4, 0.2, 0.2, 0.2)  # corresponding probabilities
nSamples = 10
# Draw nSamples with replacement
x = sample(values, nSamples, replace=TRUE, prob=probs)
print(x)

#---------------------------------------
# pmin for arrays

# pmin() (for parallel minima) takes two or more arrays of the 
# same length and returns the mininum at
# each entry

x = c(1, 4, 6, 7)
y = c(2, 3, 4, 10)
z = pmin(x, y) # z = c(1, 3, 4, 7)
print(z)
# z

x = rexp(10, 0.25)
y = rexp(10, 0.25)
z = pmax(x, y)
print(z)
