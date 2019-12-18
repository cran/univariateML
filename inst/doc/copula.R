## ----abalone-------------------------------------------------------------
library("univariateML")
head(abalone)

## ---- make_data, fig.width = 6, fig.height = 5---------------------------

data = dplyr::filter(abalone, height < 0.5)
data$age = data$rings + 1.5
hist(data$height, main =" Abalone height", xlab = "Height in mm")


## ---- models-------------------------------------------------------------
models = c("mlgumbel", "mllaplace", "mllogis", "mlnorm", "mlexp", "mlgamma", 
             "mlinvgamma", "mlinvgauss", "mlinvweibull", "mlllogis", "mllnorm", 
             "mlrayleigh", "mlweibull", "mllgamma", "mlpareto", "mlbeta", "mlkumar",
             "mllogitnorm")
length(models)

## ---- AICs---------------------------------------------------------------
#' Calculate AICs for a column 
#' @param variable The variable to calculate AICs for.
#' @return A named vector of AICs.

AICs = function(variable) {
  x = data[[variable]]
  FUN = function(name, x = NULL) tryCatch(expr = eval(call(name, x)), 
                                                 error = function(e) NULL)
  fits = Filter(Negate(is.null), sapply(X = models, FUN = FUN, x = x))
  sort(sapply(fits, AIC))
}


## ---- full_call_diamter--------------------------------------------------
AICs("age")

## ---- partial_call-------------------------------------------------------
sapply(X = c("diameter", "height", "shell_weight", "age"), 
       FUN = function(x) names(AICs(x))[1])

