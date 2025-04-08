#---------------------------------------------------------
# File:   mit18_05_s22_studio1.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# Studio 1 ----

# 1. Be sure to read the instructions file: studio1-instructions.pdf

# 2. The instruction file gives instructions for testing your code. You can see the output of the tests in studio1-test-answers.html. You should use this to verify your code is giving correct output in the format asked for.

# 3. The handouts section (right side) of our MITx course page has a link for uploading you work.

#--------------------------------------
# Problem 1: Tutorial ----
# See the problem 1 part of studio1-instructions.pdf
#
# Summary: Look at sections 1-5 of studio1-samplecode.r.
studio1_problem_1 = function() {
  cat("----------------------------------\n")
  cat("Problem 1: Tutorial.\n")

  cat("No question here.")
}

#--------------------------------------
# Problem 2: Birthdays. ----
# 2a. See the problem 2a part of studio1-instructions.pdf
#
# Summary: Use a simulation to estimate the probability that among n people in a room at least one pair of them shares a birthday.
#
# See studio1-test-answers.html for the output of test calls to this function.

studio1_problem_2a = function(ndays_in_year, npeople, ntrials) {
  cat("----------------------------------\n")
  cat("Problem 2a: Birthdays\n")

  source('mit18_05_s22_colMatches.r')

  estimated_prob = 0 # This variable is for your simulated probability

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  birthdays = matrix(sample(1:ndays_in_year, npeople*ntrials, replace=TRUE), nrow=npeople, ncol=ntrials)
  x = (colMatches(birthdays, 2))
  estimated_prob = mean(x)

  # These lines print out your answers
  cat('Problem 2a simulation results:','\n')
  cat('  Number of days in year =', ndays_in_year, '\n')
  cat('  Number of people =', npeople, '\n')
  cat('  Number of trials =', ntrials, '\n')
  cat('  Simulated probability of a match =', estimated_prob, '\n')
}

#------
# 2b. See the problem 2b part of studio1-instructions.pdf
#
# Summary: Use the function in 2a to find the minimum number of people to have a 0.50 probability that at least one pair shares a birthday
#
# See studio1-test-answers.html for the output of test calls to this function.
studio1_problem_2b = function() {
  cat("----------------------------------\n")
  cat("Problem 2b.\n")

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********

  npeople = 23  # You need to set this to the correct value.
  cat('Smallest number of people needed for a 0.5 probability:', npeople, "\n")
}

#--------------------------------------
# OPTIONAL Problem 3: Plot birthdays. ----
# ONLY IF YOU HAVE TIME.
# 3. See the problem 3 part of studio1-instructions.pdf
#
# Summary: Make a plot of the probability of a shared birthday as a function of npeople.
#
# See studio1-test-answers.html for the output of test calls to this function.
studio1_problem_3 = function(ndays_in_year, ntrials) {
  cat("----------------------------------\n")
  cat("Problem 3: Plot birthdays.\n")

  source('mit18_05_s22_colMatches.r')

  npeople = 1:100;
  N = length(npeople)
  estimated_prob = rep(0, N);
  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********
  for (np in 1:N) {
    birthdays = matrix(sample(1:ndays_in_year, np*ntrials, replace=TRUE), nrow=np, ncol=ntrials)
    estimated_prob[np] = mean(colMatches(birthdays, 2))
  }
  
  plot(npeople, estimated_prob, type="l", col="red", lwd=2)

  # The output is a graph
  cat('See plot\n')
}

#--------------------------------------
studio1_problem_3(365, 1000)

