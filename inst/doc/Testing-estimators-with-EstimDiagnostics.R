## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=7
)

## ----setup--------------------------------------------------------------------
library(EstimDiagnostics)
library(doParallel)
library(ggplot2)

registerDoSEQ()
s<-c(1e1,1e2,1e3)
Nmc=6e2

## -----------------------------------------------------------------------------
Inference<-function(s){
  rrr<-rnorm(n=s)
  list(Mn=mean(rrr), Var=var(rrr))
}

experiment <- Estim_diagnost(Nmc, s=s, Inference)
head(experiment)

## -----------------------------------------------------------------------------
estims_qqplot(experiment)
estims_boxplot(experiment)

## -----------------------------------------------------------------------------
library(gridExtra)

dist1 <- function(p) stats::qchisq(p, df=1e1)
p1<-estims_qqplot(experiment[experiment$s==1e1,], sep=TRUE, distribution = dist1)

dist2 <- function(p) stats::qchisq(p, df=1e2)
p2<-estims_qqplot(experiment[experiment$s==1e2,], sep=TRUE, distribution = dist2)

dist3 <- function(p) stats::qchisq(p, df=1e3)
p3<-estims_qqplot(experiment[experiment$s==1e3,], sep=TRUE, distribution = dist3)

grid.arrange(arrangeGrob(p1[[2]], p2[[2]], p3[[2]], ncol=2))

## -----------------------------------------------------------------------------
s <- 1e1
set.seed(1)

experiment <- Estim_diagnost(Nmc, s=s, Inference)

sam_m <- experiment[,1]
expect_mean_equal(x=sam_m, mu=0)

## -----------------------------------------------------------------------------
sam_v <- experiment[,2]*10
expect_distfit(sample = sam_v, nulldist=pchisq, df=10)

