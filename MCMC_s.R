
####
## Monte Carlo for P. L=1, D=2
n <- 10000
phi <- runif(n, min=0, max=pi)
y <- runif(n, min=0, max=1)
sum(y < 0.5*sin(phi))/n
# approximate 1/pi = 0.318 ?



####
# A function to estimate pi
pifun <- function(n) {
  x <- runif(n, min=-1, max=1)	# generate x
  y <- runif(n, min=-1, max=1)	# generate y
  L <- sqrt(x^2 + y^2)        	# distance to the origin point
  P <- sum(L < 1)/n           	# probability
  4*P
}
pifun(1000)


####
x <- seq(10-10,20,0.01)
y <- dnorm(x=x, mean=10, sd=3)
plot(x, y, type="l", main="Normal: mean = 10, sd = 3", ylab = "Density", xlab = "X", 
     cex.lab=1.5, cex.main=2)

x <- seq(0,5,0.01)
y <- dexp(x=x, rate = 0.5)
plot(x, y, type="l", main="Exponential: mean = 2", ylab = "Density", xlab = "X", 
     cex.lab=1.5, cex.main=2)

x <- 0:30
y <- dbinom(x=x, size=30, prob=0.3)
plot(x, y, type="h", main="Binomial: success rate = 0.3, 30 trials", ylab="Probability", xlab = "X", 
     cex.lab=1.5, lwd=2, cex.main=2)



#### Weak law of large numbers
# A function to calculate sample mean
samplemeanfun <- function(dist, n) {
  if (dist=="normal") {
    xbar <- mean(rnorm(n, mean=10, sd=3))
  } else if (dist=="exponential") {
    xbar <- mean(rexp(n, rate = 0.5))
  } else {
    xbar <- mean(rbinom(n, size=30, prob=0.3))
  }
  xbar
}

n <- 100
epsilon <- 0.3
set.seed(101)
xbars <- replicate(1000, samplemeanfun(dist="normal", n))
sum(abs(xbars - 10) > epsilon)/1000

set.seed(101)
xbars <- replicate(1000, samplemeanfun(dist="exponential", n))
sum(abs(xbars - 2) > epsilon)/1000

set.seed(101)
xbars <- replicate(1000, samplemeanfun(dist="binomial", n))
sum(abs(xbars - 9) > epsilon)/1000




####
# Exercise: Cauchy distribution
x <- seq(-5,5,0.01)
y <- dcauchy(x=x, location = 0, scale = 1)
plot(x, y, type="l", main="Cauchy: location = 0, scale = 1", ylab = "Density", xlab = "X", 
     cex.lab=1.5, cex.main=2)

n <- 100
epsilon <- 0.3
set.seed(101)
xbars <- replicate(1000, mean(rcauchy(n, location = 0, scale = 1)) )
sum(abs(xbars - 0) > epsilon)/1000




## Trend of sample means of cauchy distributions
set.seed(110)
maxn <- 5000
X_all <- rcauchy(maxn, location = 0, scale = 1)
xbars <- cumsum(X_all)/(1:maxn)
plot(1:maxn, xbars, type="l", ylim=c(-10,10), ylab="Xbar", xlab="Sample size", cex.lab=1.5, main="Cauchy")
abline(h=0, col="red")




####
# A function to calculate sample mean
samplemeanfun <- function(dist, n) {
  if (dist=="normal") {
    xbar <- mean(rnorm(n, mean=10, sd=3))
  } else if (dist=="exponential") {
    xbar <- mean(rexp(n, rate = 0.5))
  } else {
    xbar <- mean(rbinom(n, size=30, prob=0.3))
  }
  xbar
}

set.seed(101)
n <- 100
xbars <- replicate(1000, samplemeanfun(dist="normal", n))
hist(xbars, breaks = seq(min(xbars)-0.1,max(xbars)+0.1, 0.1), freq=FALSE, ylim=c(0,2), 
     main="Normal", xlab = "Xbar", cex.lab=1.5)
lines(density(xbars), col = "red", lwd = 3)

set.seed(101)
n <- 100
xbars <- replicate(1000, samplemeanfun(dist="exponential", n))
hist(xbars, breaks = seq(min(xbars)-0.1,max(xbars)+0.1, 0.1), freq=FALSE, ylim=c(0,2), 
     main="Exponential", xlab = "Xbar", cex.lab=1.5)
lines(density(xbars), col = "red", lwd = 3)

set.seed(101)
n <- 100
xbars <- replicate(1000, samplemeanfun(dist="", n))
hist(xbars, breaks = seq(min(xbars)-0.1,max(xbars)+0.1, 0.1), freq=FALSE, ylim=c(0,2), 
     main="Binomial", xlab = "Xbar", cex.lab=1.5)
lines(density(xbars), col = "red", lwd = 3)



####
## Exercise: 5, 10, 20, 30, 50, 100
set.seed(101)
n <- 5
xbars <- replicate(1000, samplemeanfun(dist="exponential", n))
hist(xbars, breaks = seq(min(xbars)-0.1,max(xbars)+0.1, 0.1), freq=FALSE, ylim=c(0,2), 
     main="Exponential", xlab = "Xbar", cex.lab=1.5)
lines(density(xbars), col = "red", lwd = 3)



#### Bayesian
x <- seq(0,1,0.01)
y1 <- dbeta(x=x, 5, 2)
y2 <- dbeta(x=x, 1, 1)
y3 <- dbeta(x=x, 3, 2)
plot(x, y1, type="l", main="Beta(a, b)", ylab = "Density", xlab = expression(theta), 
     cex.lab=1.5, cex.main=2, col="blue")
lines(x, y2, col="green")
lines(x, y3, col="red")
legend("topleft", legend=c("a=5, b=2", "a=1, b=1", "a=3, b=2"), col=c("blue","green","red"), lty=1, cex=1.5, bty = "n")



####
x <- seq(0.6,0.8,0.005)
y <- dbeta(x=x, (709 + 3), (1000 - 709 + 2))
plot(x, y, type="l", main=expression(paste("Posterior distribution of ", theta)), xlim=c(0.6,0.8),
     ylab = "Density", xlab = expression(theta), cex.lab=1.5, cex.main=2)

set.seed(102)
mean(rbeta(5000, (709 + 3), (1000 - 709 + 2)))






#### Metropolis-Hasting algorithm
n <- 1000; y <- 709; a <- 3; b <- 2; ap <- 3; bp <- 2

M <- 100500    # k = 1, 2,…, M
targ.fun <- function(theta0) dbinom(y, size=n, theta0)*dbeta(theta0, a, b)   #target distribution
prop.fun <- function(theta0) dbeta(theta0, ap, bp)   #proposal distribution

theta <- numeric(M)
theta[1] <- 0.5
for(k in 2:M) {
  theta.k <- theta[k-1]
  theta.cand <- rbeta(1, ap, bp)
  ratio <- targ.fun(theta.cand)*prop.fun(theta.k)/(targ.fun(theta.k)*prop.fun(theta.cand))
  p <- min(1,ratio)
  accept <- sample(c(1,0), size=1, prob=c(p, 1-p))
  theta[k] <- theta.cand*(accept==1) + theta.k*(accept==0)
}
#The 1st to 500th theta were thrown out, known as the burn-in
theta.mcmc <- theta[(500+1):M]
mean(theta.mcmc)



####
aa <- y + a
bb <- n - y + b
pdf.f <- function(theta0) dbeta(theta0, aa, bb)
theta.mcmcL <- round(min(theta.mcmc), 2) - 0.01
theta.mcmcU <- round(max(theta.mcmc), 2) + 0.01
hist(theta.mcmc, freq = FALSE, #col = "lightblue", border = "skyblue3",
     xlab = expression(paste(theta, " | ", y)), main = "",
     ylim = c(0, ceiling(pdf.f((y + a)/(a + b + n)))),
     xlim = c(theta.mcmcL, theta.mcmcU),
     breaks = seq(theta.mcmcL, theta.mcmcU, 0.01),
     cex.lab = 1.5)
lines(density(theta.mcmc, bw = 0.005), col = "red", lwd = 3)

z <- seq(0, 1, by = 0.001)
lines(z, pdf.f(z), type = "l", col = "gray34", lwd = 3, lty = 3)

legend("topright", legend = c("Theoretical", "MCMC"),
       col = c("gray34", "red"), lwd = 3, lty = c(3, 1))





#### Gibbs sampler
n <- 30 
a <- 3 
b <- 2
M <- 10^5

# generate data (yk, thetak), k=1,2,...
# but only store yk, k=1,2,...
y <- numeric(M)
y[1] <- sample(c(0:n), 1)
for (i in 2:M) {
  theta <- rbeta(1, y[i-1] + a, n - y[i-1] + b)
  y[i] <- rbinom(1, n, theta)
}
# Histogram for yk, k=1,2,...
hist(y, breaks = seq(-0.5, n + 0.5, by = 1), freq = FALSE, 
     main = "Beta-Binomial distribution")

mean(y)  # sample mean




####
### Theoretical distribution of y
px <- function(x, n, a, b) {
  choose(n, x)*gamma(a + b)*gamma(x + a)*gamma(n - x + b)/
    (gamma(a)*gamma(b)*gamma(a + b + n))
}
z <- c(0:n)
points(z, px(z, n, a, b), type = "h", col = 4, lw = 5)

legend("topleft", c("MCMC", "Theoretical"), 
       col =c(1, 4), lty = c(1, 1), lwd=2)






#### Strong law of large numbers
set.seed(100)
maxn <- 5000
X_all <- rnorm(maxn, mean=10, sd=3)
xbars <- cumsum(X_all)/(1:maxn)
plot(1:maxn, xbars-10, type="l", ylim=c(-0.7,0.7), ylab="Bias", xlab="Sample size", cex.lab=1.5, main="Normal")
abline(h=0, col="red")
lines(1:maxn, cumsum(rnorm(maxn, mean=10, sd=3))/(1:maxn)-10, col=4)


set.seed(100)
maxn <- 5000
X_all <- rexp(maxn, rate = 0.5)
xbars <- cumsum(X_all)/(1:maxn)
plot(1:maxn, xbars-2, type="l", ylim=c(-0.7,0.7), ylab="Bias", xlab="Sample size", cex.lab=1.5, main="Exponential")
abline(h=0, col="red")

set.seed(100)
maxn <- 5000
X_all <- rbinom(maxn, size=30, prob=0.3)
xbars <- cumsum(X_all)/(1:maxn)
plot(1:maxn, xbars-9, type="l", ylim=c(-0.7,0.7), ylab="Bias", xlab="Sample size", cex.lab=1.5, main="Binomial")
abline(h=0, col="red")

