## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----docs, eval = FALSE-------------------------------------------------------
#  ?actuar::dlgamma

## ----lomax, fig.width = 6, fig.height = 5-------------------------------------
eps = 0.1
x = seq(0, 3, length.out = 100)
plot(dexp, 0, 3, xlab = "x", ylab = "Density", main = "Exponential and Lomax")
lines(x, extraDistr::dlomax(x, lambda = eps, kappa = 1/eps), col = "red")

