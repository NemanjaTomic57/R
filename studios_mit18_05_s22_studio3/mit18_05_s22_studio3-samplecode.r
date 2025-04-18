#---------------------------------------------------------
# File:   mit18_05_s22_studio3-samplecode.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# Sample code for 18.05 Studio 3
#--------------------------------
# Generating random numbers from distributions
# rnorm generates random values from the normal distribution
# rnorm(n, mu, sigma) gives us n random values from Norm(mu, sigma^2)

# Examples
# 1. Simulate 5 trials with a normal variable with mu=0, sigma=1 
x = rnorm(5, 0, 1)
print(x)

# 2. Run 5 trials with mu=100, sigma=15 (so sigma^2 = 225)
x = rnorm(5, 100, 15)  
print(x)

# Likewise for the exponential distribution
# 3. Simulate 5 trials with an exponential(1) variable
x = rexp(5, 1)
print(x)

# 4. 5 trials with an exponential(2.5)
x = rexp(5, 2.5)
print(x)

# Likewise for the uniform distribution
# 5. 5 trials with a uniform variable on the interval [0, 1]
x = runif(5, 0, 1)
print(x)

# 6. 8 trials with a uniform variable on 
# the interval [0, 100]
x = runif(5, 0, 100)
print(x)

#----------------------
# In R, every distribution has functions
# r*, d*, p*, q*

# Let's look at rnorm, dnorm, pnorm, qnorm
# rnorm generates random values
# rnorm(n, mu, sigma) gives us n random values from Norm(mu, sigma^2)

x = rnorm(5, 0, 1)
print(x)

# Test the 0.68, 0.95 rules
x = rnorm(10000, 0, 1)
m1 = mean(abs(x) < 1)
m2 = mean(abs(x) < 2)
cat('Mean m1 should be near 0.68: m1 = ', m1, '\n')
cat('Mean m2 should be near 0.95: m2 = ', m2, '\n')

#--
# dnorm is the pdf: it gives the values of the 
# density at x

# Here is the standard normal density evaluated at 
# x = 0
x = 0
d = dnorm(x, 0, 1)
print(d)

# x can also be a list of values. In this
# case dnorm returns a list of density values.
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
plot(z,y, type='l', col='blue', lwd=2)

# We can add some other densities in 
# other colors
lines(z,dnorm(z, 1, 1), col='orange', lwd=2)
lines(z,dnorm(z, 0, 2), col='green', lwd=2)

#---
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
p1 = pnorm(x, 0, 1)
print(p1)

p2 = pnorm(x, 0, 2)
print(p2)

# We can use pnorm to plot the standard normal cdf
z = seq(-6, 6, 0.01)  
y = pnorm(z, 0, 1)
plot(z, y, type='l', col='blue', lwd=3)
# Add some other cdf's
lines(z,pnorm(z, 1, 1), col='orange', lwd=3)
lines(z,pnorm(z, 0, 2), col='green', lwd=3)

#---
# qnorm finds quantiles. (We will learn about these in class 6)
# It is the inverse of the cdf it takes a 
# probability and produces a value. That is,
#a = qnorm(p, 0, 1)  <==> P(Z<a) = p 
#                    <==> p = pnorm(a, 0, 1)
# Here is the inverse in action
p = 0.5 # a probability
q = qnorm(p, 0, 1)      # The x value that gives prob. p
pcheck = pnorm(q, 0, 1) # give back the original p
cat("p = ", p, "; q = ", q, "; pcheck = ", pcheck, "\n")

# Another check
q = 0.351 # a quantile
p = pnorm(q, 0, 1)
qcheck = qnorm(p, 0, 1) # get back the original q
cat("q = ", q, "; p = ", p, "; qcheck = ", qcheck, "\n")

#---
# Similarly there is runif(x, a, b) and rexp(x, lambda)
# (Likewise dunif, punif, qunif, dexp, pexp, qexp)

# Plot the pdf of exponential(0.5)
x = seq(0, 10, 0.01)
plot(x,dexp(x, 0.5), type='l', col='blue', lwd=2)
abline(v=0)
abline(h=0)

#------------------------------
#Histograms
# R makes plotting histograms of data easy

# Usually, we will set the bins explicitly as follows

rate = 0.5
data = rexp(100, rate)

# Make bins of width 0.4 that start at the minimum
# data value and go to the maximum data
bin_width = 0.4
bins = seq(min(data), max(data)+bin_width, bin_width)

# We use xlab and paste to label the x-axis
label = paste('Exponential data: rate =', rate)

# To get a density histogram we set freq=FALSE
# Notice that the vertical axis is now labeled 'Density'
hist(data, breaks=bins, col='orange', xlab=label, freq=FALSE)

#----
# With enough data the density histogram should 
# look like the pdf. We check this by drawing the 
# pdf on top of the histogram using lines()
rate = 0.5
label = paste('Exponential data: rate =', rate)
data = rexp(10000, rate)
bin_width = 0.3
bins = seq(min(data), max(data)+bin_width, bin_width)
hist(data, breaks=bins, col='orange', xlab=label, freq=FALSE)
x = seq(0, max(data)+1, 0.01)
lines(x,dexp(x, rate), col='blue', lwd=2)

#---------------------
# Here are some quick calls to hist() that set
# the bins automatically. I don't think
# R does a great job of automatically choosing bins,
# so I generally avoid using these

# Generate some random exponential data and plot a histogram
data = rexp(100, 0.5)
# Let R make the choice of the number of bins
hist(data)

# Controlling the number of bins
# When breaks = a single number it specifies
# the number of bins (actually this just 
# 'suggests' the number of bins. 
# I'm not sure how R chooses the exact number)
rate = 0.5
data = rexp(100, rate)

# Run these one at a time in RStudio
hist(data, breaks=12, col='blue')
hist(data, breaks=100,col='orange')
hist(data, breaks=2, col='cyan')
