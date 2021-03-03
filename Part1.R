hist(runif(1000))

mns = NULL
for (i in 1 : 1000) mns = c(mns, mean(runif(40)))
hist(mns)

lambda <- 0.2   # parameter - lambda
n <- 40   # number of exponential
sims <- 1000   # number of simulations

rexp(n, lambda)
sim_exp <- replicate(sims, rexp(n, lambda))  #40*1000 matrix

means_exp <- apply(sim_exp, 2, mean) #  mean of 40 numbers in 1000 times
var_exp <- apply(sim_exp, 2, var)
range(means_exp)
range(var_exp)
hist(means_exp, breaks=40, xlim = c(2,8), main="Means of Exponential Function Simulation", col = "orange")
hist(var_exp, breaks=40, xlim = c(5,82), main="Varance of Exponential Function Simulation", col = "orange")

# step 1
# the theoretical mean should be 1/lambda = 5 
mean_s <- mean(means_exp)
mean_t <- 1/lambda

hist(means_exp, main = "Sample Mean vs. Theoretical Mean", xlim =  c(2,8), breaks = 40, xlab = "Simulation Means")
abline(v = mean_s, lwd = "1", col = "blue")
abline(v = mean_t, lwd = "1", col = "red")

# step 2
# the variance of exponential distribution is  (1/lambda) / sqrt(n).
sd_s <- mean(var_exp)/n # sample standard deviation
print(paste("standard deviation of sample:", sd_s))
sd_t <- (1/lambda)/sqrt(n) # theoretical standard deviation
print(paste("theoretical standard deviation:", sd_t))

# step 3
#General Plot with ditribution curve drawn
hist(means_exp, prob=TRUE, col="aliceblue", main="Exponential Function Simulation Means", breaks=40, xlim=c(2,9), xlab = "Simulation Means")
lines(density(means_exp), lwd=3, col="red")

# Normal distribution line creation
x <- seq(min(means_exp), max(means_exp), length=n)
y <- dnorm(x, mean=1/lambda, sd=(1/lambda)/sqrt(n)) # f(x)
lines(x, y, pch=22, col="blue", lwd=3)

qqnorm(means_exp)
qqline(means_exp)
