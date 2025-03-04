## ----abalone------------------------------------------------------------------
library("univariateML")
head(abalone)

## ----make_data, fig.width = 6, fig.height = 5---------------------------------
data <- dplyr::filter(abalone, height < 0.5)
data$age <- data$rings + 1.5
data <- data[c("diameter", "height", "shell_weight", "age")]
hist(data$height, main = "Abalone height", xlab = "Height in mm")

## ----models-------------------------------------------------------------------
models <- c(
  "gumbel", "laplace", "logis", "norm", "exp", "gamma",
  "invgamma", "invgauss", "invweibull", "llogis", "lnorm",
  "rayleigh", "weibull", "lgamma", "pareto", "beta", "kumar",
  "logitnorm"
)
length(models)

## ----all_models---------------------------------------------------------------
univariateML_models

## ----margin_select------------------------------------------------------------
margin_fits <- lapply(data, model_select, models = models, criterion = "aic")

## ----AIC_copula, warning = FALSE, cache = TRUE--------------------------------
# Transform the marginals to the unit interval.
y <- sapply(seq_along(data), function(j) pml(data[[j]], margin_fits[[j]]))

# The copulas described above.
copulas <- list(
  normal = copula::normalCopula(dim = 4, dispstr = "un"),
  t = copula::tCopula(dim = 4, dispstr = "un"),
  joe = copula::joeCopula(dim = 4),
  clayton = copula::claytonCopula(dim = 4),
  gumbel = copula::gumbelCopula(dim = 4)
)

fits <- sapply(
  copulas,
  function(x) copula::fitCopula(x, data = y, method = "mpl")
)

sapply(fits, AIC)

