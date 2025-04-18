#---------------------------------------------------------
# File:   mit18_05_s22_studio3.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# Studio 3 ----

# 1. Be sure to read the instructions file!

# 2. The instruction file gives instructions for testing your code. You can see the output of the tests in studio3-test-answers.html. You should use this to verify your code is giving correct output in the format asked for.

# 3. The handouts section (right side) of our MITx course page has a link for uploading you work.

#--------------------------------------
# Problem 1: Histograms. ----
# See the problem 1 instructions for this studio.
#
# Summary: You will make frequency and density histograms.
# 
# 1a. Frequency histogram 
# See the problem 1a instructions for this studio.
# Summary: Make a frequency histogram of simulated exponential data
# See the test answers file for the output of test calls to this function
studio3_problem_1a = function(rate, nsamples) {
  cat("\n----------------------------------\n")
  cat("1a. Frequency histogram\n")
  
  # Arguments:
  #   rate = rate parameter for an exponential distribution.
  #   nsamples = the size of the sample to use for the histogram

  # We will give you the bin_width to use in the histogram
  bin_width = 0.5

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  data = rexp(nsamples, rate)
  bins = seq(min(data), max(data) + bin_width, bin_width)
  label = paste("Exponential rate:", rate)
  hist(data, breaks = bins, xlab = label, col="yellow", xlim = c(0, 10))
  
  cat('See plot\n')
}

# Problem 1b: Density histogram ----
# See the problem 1b instructions for this studio.
# Summary: Make a density histogram of simulated exponential data
#          Add a graph of the pdf of exp(rate) to your plot
# See the test answers file for the output of test calls to this function
studio3_problem_1b = function(rate, nsamples) {
  cat("-----\nProblem 1b: Density histogram\n")
  
  # Arguments:
  #   rate = rate parameter for an exponential distribution.
  #   nsamples = the size of the sample to use for the histogram

  # We will give you the bin_width to use in the histogram
  bin_width = 0.5
  
  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  data = rexp(nsamples, rate)
  bins = seq(min(data), max(data) + bin_width, bin_width)
  label = paste("Exponential rate:", rate)
  hist(data, breaks = bins, xlab = label, col="yellow", freq = FALSE)
  
  x_max = max(data)
  x = seq(0, x_max, 0.01)
  y = dexp(x, rate)
  lines(x, y, col="blue", lwd=2)

  cat('See plot\n')
}
  
#-------------------------
# Problem 2: Density histogram. ----
# See the problem 2 instructions for this studio.
# Summary: Make a density histogram of average of independent exponential data

# 2a. See the problem 2a instructions for this studio.
# Summary: Average samples from two independent exponential variables
# See the test answers file for the output of test calls to this function
studio3_problem_2a = function(rate, nsamples) {
  cat("\n----------------------------------\n")
  cat("Problem 2a: Density histogram\n")

  # Arguments:
  #   rate = rate parameter for an exponential distribution.
  #   nsamples = the size of the exponential sample

  # We will give you the bin_width to use in the histogram
  bin_width = 0.4

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  data1 = rexp(nsamples, rate)
  data2 = rexp(nsamples, rate)
  data_ave = (data1 + data2) / 2
  bins = seq(min(data_ave), max(data_ave) + bin_width, bin_width)
  label = paste("Exponential rate: ", rate)
  hist(data_ave, breaks = bins, col="blue", xlab = label, freq = FALSE)
  
  cat('See plot\n')
}

# 2b. See the problem 2b instructions for this studio.
# Summary: Average samples from n independent exponential variables
# See the test answers file for the output of test calls to this function
studio3_problem_2b = function(rate, nsamples, n_to_average, bin_width) {
  cat("\n----------------------------------\n")
  cat("-----Problem 2b: Density histogram of average\n")
  
  # Arguments:
  #   rate = rate parameter for an exponential distribution.
  #   nsamples = the size of each exponential sample
  #   n_to_average = the number exponential samples to average
  #   bin_width = the width of the bins to use in the histogram  

  # mean and stdandard deviation of the average
  # of n exponential random variables
  mean_of_average = 1/rate  
  std_dev_of_average = (1/rate)/sqrt(n_to_average)
  
  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  data = matrix(rexp(nsamples * n_to_average, rate), nrow = nsamples, ncol = n_to_average)
  data_ave = rowSums(data) / n_to_average
  bins = seq(min(data_ave), max(data_ave) + bin_width, bin_width)
  label = paste("Exponential rate:", rate)
  hist(data_ave, breaks = bins, xlab = label, col = "blue", freq = FALSE)
  
  x_max = max(data_ave)
  x = seq(0, x_max, 0.01)
  y = dnorm(x, mean = mean_of_average, sd = std_dev_of_average)
  print(y)
  lines(x, y, col="red", lwd=2)
  
  cat('See plot\n')
}

source('mit18_05_s22_ps3prob2-data.r')
studio3_ps_2 = function() {
  x = get_prob2_data()
  
  x_mean = mean(x)
  x_var = var(x)
  x_sd = sqrt(x_var)
  
  bin_width = 0.1
  bins = seq(min(x), max(x) + bin_width, bin_width)
  hist(x, breaks = bins, col="yellow", xlab = "years")
}

studio3_ps_2()

