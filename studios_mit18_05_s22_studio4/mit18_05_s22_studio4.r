#---------------------------------------------------------
# File:   mit18_05_s22_studio4.r 
# Authors: Jeremy Orloff and Jennifer French
# Updated for clarity and performance
#---------------------------------------------------------

# Problem 1a: Simulated covariance and correlation
studio4_problem_1a = function(n_together, n_Barto_alone, ntrials) {
  cat("\n----------------------------------\n")
  cat("Problem 1a. Barto and Axel simulated covariance and correlation.\n")
  
  # Probabilities for +1 and -1 outcomes
  probabilities = c(18/38, 20/38)
  
  axel_totals = numeric(ntrials)
  barto_totals = numeric(ntrials)
  
  # Simulate each trial
  for (i in 1:ntrials) {
    together_outcomes = sample(c(1, -1), size = n_together, replace = TRUE, prob = probabilities)
    barto_alone_outcomes = sample(c(1, -1), size = n_Barto_alone, replace = TRUE, prob = probabilities)
    
    axel_totals[i] = sum(together_outcomes)
    barto_totals[i] = sum(together_outcomes) + sum(barto_alone_outcomes)
  }
  
  # Compute statistics
  mean_axel = mean(axel_totals)
  mean_barto = mean(barto_totals)
  var_axel = var(axel_totals)
  var_barto = var(barto_totals)
  cov_together = cov(axel_totals, barto_totals)
  cor_together = cor(axel_totals, barto_totals)
  
  # Print results
  cat("For n_together =", n_together, "and n_Barto_alone =", n_Barto_alone, "we have:\n")
  cat("Axel's sample mean is", mean_axel, '\n')
  cat("Barto's sample mean is", mean_barto, '\n')
  cat("Axel's sample variance is", var_axel, '\n')
  cat("Barto's sample variance is", var_barto, '\n')
  cat("Their sample covariance is", cov_together, '\n')
  cat("Their sample correlation is", cor_together, '\n')
}


# Problem 1b: Description of behavior
studio4_problem_1b = function() {
  cat("-----\n")
  cat("1b. Describe behavior as n_Barto_alone increases\n")
  
  cat("As n_Barto_alone increases, the part of Barto's outcome that is shared with Axel becomes a smaller proportion of his total.\n")
  cat("This reduces the shared variance, so the covariance decreases.\n")
  cat("Since the correlation is the normalized covariance, it also tends to zero.\n")
}

# Problem 2: Simulated central limit theorem
studio4_problem_2 = function(n_bets_per_trial, ntrials) {
  cat("\n----------------------------------\n")
  cat("Problem 2. Simulated central limit theorem.\n")
  
  probability = c(18/38, 20/38)
  
  # Run simulation: each trial gives the mean of a number of bets
  trial_means = replicate(ntrials, mean(sample(c(1000, -1000), n_bets_per_trial, replace = TRUE, prob = probability)))
  
  # Plot histogram with overlaid normal distribution
  hist(trial_means, col = "orange", freq = FALSE, main = "Central Limit Theorem Simulation", xlab = "Average Win per Trial")
  
  # Overlay normal distribution
  x_vals = seq(min(trial_means), max(trial_means), length.out = 1000)
  normal_density = dnorm(x_vals, mean = mean(trial_means), sd = sd(trial_means))
  lines(x_vals, normal_density, col = "red", lwd = 2)
  
  cat('See plot\n')
}

# Example test run
studio4_problem_1a(10, 8, 10000)
