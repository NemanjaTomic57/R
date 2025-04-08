problem_1 = function() {
  cat("--------------------------------\n")
  cat("Problem 1\n")
  cat("Two-pairs with 6 card hand\n")
  
  deck <- matrix(0, nrow=2, ncol=52)
  index <- 1
  
  for (suite in 1:4) {
    for (rank in 1:13) {
      deck[1, index] <- suite
      deck[2, index] <- rank
      index <- index + 1
    }
  }
  
  # Create a hand with suite and rank
  ntwo_pairs = 0
  nhands = 1000
  
  for (int in 1:nhands) {
    hand <- deck[, sample(ncol(deck), 6)]
    
    # Check if hand has a pair and count up if that is the case
    if (sum(table(hand[1,]) == 2)) {
      ntwo_pairs <- ntwo_pairs + 1
    }
  }
  print(ntwo_pairs / nhands)
}

problem_3e = function() {
  source("mit18_05_s22_colMatches.r")
  
  # Set up parameters
  ndays = 365
  npeople = 20
  ntrials = 1000
  size_match = 2
  
  year = 1:ndays
  
  # Run ntrials - one per column - using sample() and matrix()
  y = sample(year, npeople*ntrials, replace=TRUE)
  trials = matrix(y, nrow=npeople, ncol=ntrials)
  w = colMatches(trials, size_match)
  prob_B = mean(w)
  print(prob_B)
}

problem_3g = function() {
  source("mit18_05_s22_colMatches.r")
  
  # Set up parameters
  ndays = 365
  npeople = 88
  ntrials = 100000
  size_match = 3
  
  year = 1:ndays
  
  # Run ntrials - one per column - using sample() and matrix()
  y = sample(year, npeople*ntrials, replace=TRUE)
  trials = matrix(y, nrow=npeople, ncol=ntrials)
  w = colMatches(trials, size_match)
  prob_B = mean(w)
  print(prob_B)
}

problem_3g()
