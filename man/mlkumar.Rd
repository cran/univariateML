% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mlkumar.R
\name{mlkumar}
\alias{mlkumar}
\title{Kumaraswamy distribution maximum likelihood estimation}
\usage{
mlkumar(x, na.rm = FALSE, ...)
}
\arguments{
\item{x}{a (non-empty) numeric vector of data values.}

\item{na.rm}{logical. Should missing values be removed?}

\item{...}{\code{a0} is an optional starting value for the \code{a} parameter.
\code{rel.tol} is the relative accuracy requested, defaults
to \code{.Machine$double.eps^0.25}. \code{iterlim} is a positive integer
specifying the maximum number of iterations to be performed before the
program is terminated (defaults to \code{100}).}
}
\value{
\code{mlkumar} returns an object of \link[base:class]{class} \code{univariateML}.
This is a named numeric vector with maximum likelihood estimates for \code{a}
and \code{b} and the following attributes:
\item{\code{model}}{The name of the model.}
\item{\code{density}}{The density associated with the estimates.}
\item{\code{logLik}}{The loglikelihood at the maximum.}
\item{\code{support}}{The support of the density.}
\item{\code{n}}{The number of observations.}
\item{\code{call}}{The call as captured my \code{match.call}}
}
\description{
Uses Newton-Raphson to estimate the parameters of the Kumaraswamy
distribution.
}
\details{
For the density function of the Kumaraswamy distribution see
\link[extraDistr:Kumaraswamy]{Kumaraswamy}.
}
\examples{
AIC(mlkumar(USArrests$Rape / 100))
}
\references{
Jones, M. C. "Kumaraswamy's distribution: A beta-type
distribution with some tractability advantages." Statistical Methodology
6.1 (2009): 70-81.

Kumaraswamy, Ponnambalam. "A generalized probability density function
for double-bounded random processes." Journal of Hydrology 46.1-2 (1980):
79-88.
}
\seealso{
\link[extraDistr:Kumaraswamy]{Kumaraswamy} for the Kumaraswamy density.
}
