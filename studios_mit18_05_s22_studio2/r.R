simulate_longest_run <- function(ntrials = 10000, nflips = 50, run_threshold = 8) {
  trials <- matrix(rbinom(ntrials * nflips, 1, 0.5), nrow = ntrials)
  longest_runs <- apply(trials, 1, function(x) max(rle(x)$lengths))
  
  avg_longest_run <- mean(longest_runs)
  prob_run_threshold <- mean(longest_runs >= run_threshold)
  
  cat("Average longest run in", nflips, "flips:", avg_longest_run, "\n")
  cat("Probability of a run of", run_threshold, "or more:", prob_run_threshold, "\n")
}

simulate_longest_run()