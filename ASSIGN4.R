#Q1 Prob discrete
x <- c(0, 1, 2, 3, 4)
p_x <- c(0.41, 0.37, 0.16, 0.05, 0.01)

expected_value_1 <- sum(x * p_x)
print(expected_value_1)

expected_value_2 <- weighted.mean(x, p_x)
print(expected_value_2)

expected_value_3 <- c(x %*% p_x)
print(expected_value_3)

#Q2 cont prob dist
f<-function(t)
    {
    return (t*0.1*exp(-0.1*t))
    }
cat(integrate(f,0,Inf)$value,"\n")

#Q3 same concept of q1
Y_values <- c(-12, -2, 8, 18)
probabilities <- c(0.1, 0.2, 0.2, 0.5)
expected_Y <- sum(Y_values * probabilities)
cat(expected_Y,"\n")

#Q4 first moment=mean =int x*f(x)  second=x^2*f(x) var =second-first^2
f <- function(x) { x^0.5 * exp(-x) }
f1 <- function(x) { x^2 * 0.5 * exp(-x) }
E_X <- integrate(f, lower = 1, upper = 10)$value
E_X2 <- integrate(f1, lower = 1, upper = 10)$value
Var_X <- E_X2 - (E_X)^2
cat("E[X] (Mean): ", E_X," E[X^2] (Second Moment): ", E_X2," Var(X) (Variance): ", Var_X, "\n")

#Q5 geometric dist
p_x <- function(x) (3/4) * (1/4)^(x-1)
p_y <- function(y) { x <- sqrt(y); if (x == floor(x)) p_x(x) else 0 }

cat("P(Y = 9):", p_y(9), "\n")

x_vals <- 1:5
y_vals <- x_vals^2
p_Y_vals <- sapply(y_vals, p_y)

E_Y <- sum(y_vals * p_Y_vals)
Var_Y <- sum(y_vals^2 * p_Y_vals) - E_Y^2

cat("E[Y]:", E_Y, "\nVar(Y):", Var_Y, "\n")
