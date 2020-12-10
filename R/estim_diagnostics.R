
estim_output_converter <- function(LL){

  if(is.vector(LL,mode = 'numeric')) {

    as.data.frame(t(LL))

  } else {

    if(is.list(LL)) {

      as.data.frame(LL)

    } else {

      if(is.data.frame(LL)){

        if(nrow(LL)==1) {

          LL

        } else {

          if(ncol(LL)==1) t(LL)

        }

      }

    }

  }

}


#' Sample estimators' values for different sample sizes
#'
#' For every sample size value the function creates a sample and evaluates the estimators Nmc times.
#' @return data frame with estimators' values
#' @param Nmc number of repetitions
#' @param s numeric vector of sample sizes
#' @param Inference function of s creating a sample and evaluating estimators (see details)
#' @param packages list of packages to pass to foreach loop
#' @examples
#' Nmc=400
#' s<-c(1e2,1e3)
#'
#' Inference<-function(s){
#'   rrr<-rnorm(n=s)
#'   list(Mn=mean(rrr), Sd=sd(rrr))
#' }
#' data <- Estim_diagnost(Nmc, s, Inference)
#' estims_qqplot(data)
#' estims_boxplot(data)
#'
#' #
#' Inference<-function(s){
#' rrr<-2/0
#' list(Mn=mean(rrr), Sd=sd(rrr))
#' }
#' head(Estim_diagnost(Nmc, s, Inference))
#'
#' #
#' Inference<-function(s){
#' rrr<-rnorm(n=s)
#' rrr[2]<-"dwq"
#' list(Mn=mean(rrr), Sd=sd(rrr))
#' }
#' head(Estim_diagnost(Nmc, s, Inference))
#'
#' @export
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
Estim_diagnost <- function(Nmc, s, Inference, packages=NULL){

  i <- integer(0)
  ind <- integer(0)
  data_whole <- data.frame()
  s_whole <- integer(0) # factor vector with s for data_whole

  for (i in 1:length(s)) {

    data <- matrix()

    # sample simulation + statistics extraction
    data <- foreach(ind = 1:Nmc, .combine = rbind, .packages = packages, .inorder = TRUE) %dopar% {

      # Inference must return a number, or NA or NaN for every parameter, or throw an error
      # c(1,2,'abcd') will result in halting of execution

      LL <- tryCatch(Inference(s=s[i]), error=function(c) 'Inference produced error(s)')

      if (!is.character(LL)) # find a character in one element at least!
        estim_output_converter(LL)
    }

    if (!is.null(data) & !is.null(names(data))) {

      data <- as.matrix(data) #!!!! this double-conversion seems to work best
      vect_s<-rep(s[i], length.out=nrow(data))

      # what we will aggregate
      data_whole <- rbind(data_whole,data)
      s_whole <- c(s_whole,vect_s)
    }

    else {
      # there used to be stop function instead of warning
      warning("The inference function either hasn't produced any data, or the estimates have no names")
    }
  }

  nms <- names(data_whole)
  data_whole$s <- s_whole
  data_whole
}

#################################
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 stat_qq
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 theme_bw
estims_qqplot_joint <- function(data, ...)
{
  s<-NA; value<-NA
  data_molt<-reshape2::melt(data, id.vars="s")
  ggplot2::ggplot(data_molt, ggplot2::aes(sample = value, colour = factor(s))) +
    ggplot2::stat_qq(...) +
    ggplot2::facet_wrap(.~variable, scales = "free", labeller = "label_both") +
    ggplot2::theme_bw()
}


estims_qqplot_sep <- function(data, ...)
{
  s<-NA; value<-NA
  pl_list<-list()
  cns <-colnames(data)
  not_s <- !(cns == 's')
  value_inds <- which(not_s)

  for (ind in 1:length(value_inds)) {

    curr_name_ind <- colnames(data)[ind]
    plot_values <- data[,curr_name_ind]
    data_curr <- data.frame(cbind(plot_values, s=data$s))
    pl <- ggplot2::ggplot(data_curr, ggplot2::aes(sample = plot_values, colour = factor(s)))
    pl <- pl +
          ggplot2::stat_qq(...) +
          ggplot2::theme_bw()

    pl_list <- c(pl_list, list(pl))
  }
  pl_list
}

#' QQ-plot of estimator empirical distributions
#'
#' Plot QQ-plots of estimators' empirical distributions for different sample sizes.
#' @return ggplot2 object
#' @param data data frame returned by \code{\link{Estim_diagnost}}
#' @param ... parameters to pass to stat_qq function
#' @inheritParams estims_boxplot
#' @examples
#' library(ggplot2)
#' Nmc=500
#' s<-c(1e3,4e3)
#'
#' Inference<-function(s){
#'   rrr<-rnorm(n=s)
#'   list(Mn=mean(rrr), Sd=sd(rrr))
#' }
#'
#' data <- Estim_diagnost(Nmc, s, Inference)
#' lisst <- estims_qqplot(data, sep=TRUE)
#' lisst[2][[1]] + geom_abline(intercept = 1)
#'
#' pl_joint<-estims_qqplot(data)
#' pl_joint + geom_abline(slope=1)
#'
#' pl_joint<-estims_qqplot(data, distribution = stats::qt, dparams = list(df=3, ncp=0.1))
#' pl_joint + geom_abline(slope=1)
#' @export
#'
estims_qqplot <- function(data, sep=FALSE, ...)
{
  if(sep) estims_qqplot_sep(data, ...) else estims_qqplot_joint(data, ...)
}

#################################
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
estims_boxplot_sep <- function(data)
{
  s<-NA;
  pl_list<-list()
  cns <-colnames(data)
  not_s <- !(cns == 's')
  value_inds <- which(not_s)

  for (ind in 1:length(value_inds)) {

    curr_name_ind <- colnames(data)[ind]
    plot_values <- data[,curr_name_ind]
    data_curr <- data.frame(cbind(plot_values, s=data$s))
    pl <- ggplot2::ggplot(data_curr, ggplot2::aes(factor(s), plot_values)) +
      ggplot2::geom_boxplot() +
      ggplot2::theme_bw() +
      ggplot2::xlab("Sample size") +
      ggplot2::ylab(curr_name_ind)

    pl_list <- c(pl_list, list(pl))
  }
  pl_list
}

estims_boxplot_joint <- function(data)
{
  s<-NA; value<-NA
  data_molt<-reshape2::melt(data, id.vars="s")
  ggplot2::ggplot(data_molt, ggplot2::aes(factor(s), value)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(.~variable, scales = "free")+ #, labeller = label_both) +
    ggplot2::theme_bw() +
    ggplot2::xlab("Sample size")
}

#' Boxplot of estimates
#'
#' Plot boxplots of estimators for different sample sizes.
#' @return ggplot2 object
#' @param data data frame returned by \code{\link{Estim_diagnost}}
#' @param sep indicates whether all plots will be stacked together or returned as elements of a list
#' @examples
#' Nmc=400
#' s<-seq(from = 1, to = 10, by = 2)*1e3
#' Inference<-function(s){
#' rrr<-rnorm(n=s)
#' list(Mn=mean(rrr), Sd=sd(rrr))
#' }
#'
#' data <- Estim_diagnost(Nmc, s, Inference)
#' estims_boxplot(data)
#'
#' estims_boxplot(data, sep=TRUE)
#' @export
#'
estims_boxplot <- function(data, sep=FALSE)
{
  if(sep) estims_boxplot_sep(data) else estims_boxplot_joint(data)
}
