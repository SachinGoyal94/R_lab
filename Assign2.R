#Q1 part a
# Define the total number of coins
gold <- rep("gold", 20)
silver <- rep("silver", 30)
bronze <- rep("bronze", 50)

coins <- c(gold, silver, bronze)

# Sample 10 coins randomly without replacement
sample_space <- sample(coins, 10, replace = FALSE)

# Print the sampled coins
print(sample_space)

#Q2
simulate_birthday_probability <- function(n, simulations = 1000) {
  matches <- 0
  for (i in 1:simulations) {
    birthdays <- sample(1:365, n, replace = TRUE)
    if (any(duplicated(birthdays))) {
      matches <- matches + 1
    }
  }
  prob <- matches / simulations
  return(prob)
}

n_values <- 1:50  # Test n from 1 to 50
probabilities <- sapply(n_values, simulate_birthday_probability)

plot(n_values, probabilities, type = "b", col = "blue",
     xlab = "Number of People (n)", ylab = "Probability of Shared Birthday",
     main = "Probability of Shared Birthday vs. n")

abline(h = 0.5, col = "red", lty = 2)  # Add line for p = 0.5

#part b
simulate_birthday_probability <- function(n, simulations = 1000) {
  matches <- 0
  for (i in 1:simulations) {
    birthdays <- sample(1:365, n, replace = TRUE)
    if (any(duplicated(birthdays))) {
      matches <- matches + 1
    }
  }
  prob <- matches / simulations
  return(prob)
}

n_values <- 1:50  # Test n from 1 to 50
probabilities <- sapply(n_values, simulate_birthday_probability)

n_threshold <- min(n_values[probabilities > 0.5])

cat("The smallest number of n for which the probability of a shared birthday exceeds 0.5 is:", n_threshold, "\n")


\

