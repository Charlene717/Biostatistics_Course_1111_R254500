

#### A figure
set.seed(2020)
x <- c(seq(-2*pi, -pi, 0.1), seq(-0.85*pi, -0.12*pi, 0.1), seq(0, pi, 0.1))
noise <- rnorm(length(x), 0, sd=1)
y <- sin(x) + noise
plot(x, y, pch=16)


#### Pearson correlation coefficient
fit <- lm(y~x)   # fit a line
lines(x, fitted(fit), col="red", lwd=3)

cor.test(x, y)    # Pearson correlation coefficient


#### A New Coefficient Of Correlation
library(XICOR)
xicor(x, y, pvalue=TRUE)

# Denoise
noise <- 0
y <- sin(x) + noise
plot(x, y, pch=16, col="blue")


