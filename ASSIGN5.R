#Q1
min_val <- 0
max_val <- 60

p_a <- 1 - punif(45, min_val, max_val)
p_b <- punif(30, min_val, max_val) - punif(20, min_val, max_val)

cat("P(X > 45):", p_a, "\n")
cat("P(20 ≤ X ≤ 30):", p_b, "\n")

#Q2 
#part1
lambda <- 1/2
density_at_3 <- dexp(3, rate = lambda)
cat("Density at x = 3:", density_at_3, "\n")

#part2
lambda <- 1/2
x_vals <- seq(0, 5, length.out = 100)
y_vals <- dexp(x_vals, rate = lambda)
plot(x_vals, y_vals, type = "l", col = "blue", lwd = 2,
     main = "Exponential Probability Density Function",
     xlab = "Time (hours)", ylab = "Density")

#part3
lambda <- 1/2
probability_at_most_3 <- pexp(3, rate = lambda)
cat("P(X ≤ 3):", probability_at_most_3, "\n")

#part 4
lambda <- 1/2
x_vals <- seq(0, 5, length.out = 100)
y_cdf <- pexp(x_vals, rate = lambda)
plot(x_vals, y_cdf, type = "l", col = "red", lwd = 2,
     main = "Cumulative Exponential Probability Function",
     xlab = "Time (hours)", ylab = "Cumulative Probability")

#part 5
lambda<-1/2
sim_data <- rexp(1000, rate = lambda)
hist(sim_data, breaks = 30, col = "skyblue", border = "white",
     main = "(e) Histogram of 1000 Simulated Exponential Values",
     xlab = "Simulated Repair Times (hours)")

#Q3

# Parameters
shape <- 2
beta<-1/3
rate <- 1/beta

# (a) Probability that lifetime is at least 1 unit
p_atleast_1 <- 1 - pgamma(1, shape = shape, rate = rate)
cat("(a) P(X ≥ 1) =", p_atleast_1, "\n")

# (b) Find value of c where P(X ≤ c) ≥ 0.70
c_val <- qgamma(0.70, shape = shape, rate = rate)
cat("(b) Value of c such that P(X ≤ c) ≥ 0.70 is:", c_val, "\n")
