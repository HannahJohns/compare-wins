#'  @description
#'  A rudimentary implementation of generalised pairwise comparisons for trend.
#'
#' @usage gpct(x,y, split=F)
#'
#'
#' @param K A n by n matrix of pairwise comparisons under a win statistics
#' setup. 1 indicates row i wins against column j. -1 indicates loss. 0 indicates tie
#' @param x A vector of length n containing an explanatory variable
#' @param split a boolean indicating if ties should be split between being considered
#'              a win/loss or ignored (i.e. estimating win odds or win ratio)
#'
#' @details
#' This implementation will be extremely slow and will ultimately be replaced by
#' a dedicated package implementing the method. It is however sufficient for now.
#'
#' @returns A named vector of length 2 giving the estimate and standard error for gpct under log transformation
#'
#' @references Johns, Hannah, et al. "Generalised pairwise comparisons for trend: An extension to the win ratio and win odds for dose-response and prognostic variable analysis with arbitrary statements of outcome preference." Statistical Methods in Medical Research 32.3 (2023): 609-625.
#'
#' @example
#'
#'  y <- c(rnorm(10), rnorm(10,mean = 1))
#'  K <- sign(outer(y,y,"-"))
#'
#'  x = c(rep(0,10),rep(1,10))
#'
#'  gpct(K,x)
#'
gpct <- function(K,x, split=F){

  L <- sign(outer(x,x,"-"))
  L[L==0] <- NA

  Rs = apply(K*L,1,function(x){1/length(x)*sum(ifelse(x==1,1,0),na.rm=T)})
  Rd = apply(K*L,1,function(x){1/length(x)*sum(ifelse(x==-1,1,0),na.rm=T)})
  Rt = apply(K*L,1,function(x){1/length(x)*sum(ifelse(x==0,1,0),na.rm=T)})


  if(split){
    Rs <- Rs + 0.5*Rt
    Rd <- Rd + 0.5*Rt
  }

  Pc = sum(Rs)/length(Rs)
  Pd = sum(Rd)/length(Rd)

  effect <- Pc/Pd
  se <- 2/Pd * sum(1/length(x) * (effect * Rd - Rs)^2  / length(x) )^(1/2)

  se

  lnse <- se/effect

  lneffect <- log(effect)


  c(effect=lneffect,se=lnse)
}


