% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estim_diagnostics.R
\name{estims_qqplot}
\alias{estims_qqplot}
\title{QQ-plot of estimator empirical distributions}
\usage{
estims_qqplot(data, sep = FALSE, ...)
}
\arguments{
\item{data}{data frame returned by \code{\link{Estim_diagnost}}}

\item{sep}{indicates whether all plots will be stacked together or returned as elements of a list}

\item{...}{parameters to pass to stat_qq function}
}
\value{
ggplot2 object
}
\description{
Plot QQ-plots of estimators' empirical distributions for different sample sizes.
}
\examples{
library(ggplot2)
Nmc=500
s<-c(1e3,4e3)

Inference<-function(s){
  rrr<-rnorm(n=s)
  list(Mn=mean(rrr), Sd=sd(rrr))
}

data <- Estim_diagnost(Nmc, s, Inference)
lisst <- estims_qqplot(data, sep=TRUE)
lisst[2][[1]] + geom_abline(intercept = 1)

pl_joint<-estims_qqplot(data)
pl_joint + geom_abline(slope=1)

pl_joint<-estims_qqplot(data, distribution = stats::qt, dparams = list(df=3, ncp=0.1))
pl_joint + geom_abline(slope=1)
}
