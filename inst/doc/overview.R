## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.width = 6, fig.height = 5) 

## ---- egypt-------------------------------------------------------------------
library("univariateML")
head(egypt)
hist(egypt$age, main = "Mortality in Ancient Egypt", freq = FALSE)

## ---- AIC---------------------------------------------------------------------
AIC(mlbetapr(egypt$age),
    mlexp(egypt$age),
    mlinvgamma(egypt$age),
    mlgamma(egypt$age),
    mllnorm(egypt$age),
    mlrayleigh(egypt$age),
    mlinvgauss(egypt$age),
    mlweibull(egypt$age),
    mlinvweibull(egypt$age),
    mllgamma(egypt$age))

## ----print--------------------------------------------------------------------
mlweibull(egypt$age)

## ----summary------------------------------------------------------------------
summary(mlweibull(egypt$age))

## ----model_select-------------------------------------------------------------
model_select(egypt$age, models = c("gamma", "weibull"))

## ---- qqplots-----------------------------------------------------------------
qqmlplot(egypt$age, mlweibull, datax = TRUE, main = "QQ Plot for Ancient Egypt")
# Can also use qqmlplot(mlweibull(egypt$age), datax = TRUE) directly.
qqmlpoints(egypt$age, mlgamma, datax = TRUE, col = "red")
qqmlline(egypt$age, mlweibull, datax = TRUE)
qqmlline(egypt$age, mlgamma, datax = TRUE, col = "red")

## ----plot_example, echo = TRUE------------------------------------------------
hist(egypt$age, main = "Mortality in Ancient Egypt", freq = FALSE)
lines(mlweibull(egypt$age), lwd = 2, lty = 2, ylim = c(0, 0.025))
lines(mlgamma(egypt$age), lwd = 2, col = "red")
rug(egypt$age)


## ----bootstrap_example, echo = TRUE-------------------------------------------
# Calculate two-sided 95% confidence intervals for the two Gumbel parameters.
bootstrapml(mlweibull(egypt$age)) # same as confint(mlweibull(egypt$age))
bootstrapml(mlgamma(egypt$age))

## ----bootstrap_example_mean, echo = TRUE--------------------------------------
# Calculate two-sided 90% confidence intervals for the mean of a Weibull.
bootstrapml(mlweibull(egypt$age), 
            map = function(x) x[2]*gamma(1 + 1/x[1]), 
            probs = c(0.05, 0.95))
# Calculate two-sided 90% confidence intervals for the mean of a Gamma.
bootstrapml(mlgamma(egypt$age), 
            map = function(x) x[1]/x[2],
            probs = c(0.05, 0.95))

## ----bootstrap_example_median, echo = TRUE------------------------------------
# Calculate two-sided 90% confidence intervals for the two Gumbel parameters.
bootstrapml(mlweibull(egypt$age), 
            map = function(x) qweibull(0.5, x[1], x[2]), 
            probs = c(0.05, 0.95))
bootstrapml(mlgamma(egypt$age), 
            map = function(x) qgamma(0.5, x[1], x[2]), 
            probs = c(0.05, 0.95))

## ----bootstrap_example_histogram, echo = TRUE---------------------------------

hist(bootstrapml(mlweibull(egypt$age), 
                 map = function(x) x[2]*gamma(1 + 1/x[1]), 
                 reducer = identity),
     main = "Bootstrap Samples of the Mean",
     xlab = "x",
     freq = FALSE)

## ----random variables---------------------------------------------------------
set.seed(313)
rml(10, mlweibull(egypt$age))

## ----cumulative probability---------------------------------------------------
set.seed(313)
obj = mlweibull(egypt$age)
q = seq(0, max(egypt$age), length.out = 100)
plot(q, pml(q, obj), type = "l", ylab = "Cumulative Probability")
r = rml(100, obj)
lines(ecdf(r))

