##' ordinal Bradley-Terry-Luce model (ordBTL)
##' 
##' Fits ordinal regression models to paired comparison data.
##' 
##' @usage ordBTL(formula, data, family=c("cumulative","acat"), family.control = list(), restrict=NULL, ...)
##' 
##' @param formula a formula describing the model to be fitted.
##' @param data a data frame containing the design matrix for the model 
##' (See also \code{\link{design}} to generate such an design matrix).
##' @param family a character specifying which ordinal BTL model should be fitted. 
##' Can be either \code{"cumulative"} for the cumulative link model or \code{"acat"} for the adjacent categories model.
##' @param family.control a list with arguments passed to the corresponding \code{family},
##' either \code{\link[VGAM]{cumulative}} for the cumulative link model or \code{\link[VGAM]{acat}} for the adjacent categories model.
##' @param restrict (optional) a character vector specifying the covariates from \code{formula} that should be fitted with a symmetry constraint (can be used to fit threshold covariates).
##' @param ... further arguments for fitting function (currently either 
##' \code{\link[VGAM]{vglm}} or \code{\link[VGAM]{vgam}}).
##'
##' @author Giuseppe Casalicchio
##' 
##' @return An object of class 
##' \item{\code{vglm}}{if no smoothing spline is used in the \code{formula} argument
##' (see \code{\link[VGAM]{vglm-class}}).}
##' \item{\code{vgam}}{if a smoothing spline with the function \code{\link[VGAM]{s}}
##' is used in the \code{formula} argument (see \code{\link[VGAM]{vgam-class}}).}
##' 
##' @cite agresti1992analysis
##' @cite dittrich2001modelling
##' 
##' @seealso 
##' \code{\link[VGAM]{s}}, 
##' \code{\link[VGAM]{vgam}},
##' \code{\link[VGAM]{vglm}}, 
##' \code{\link[ordBTL]{design}}, 
##' \code{\link[VGAM]{plotvgam}}
##' 
##' @example inst/examples/ordBTL_ex.R
##' @export 

ordBTL <- function(formula, data, family=c("cumulative","acat"), 
                   family.control = list(), restrict=NULL, ...){

  family <- match.arg(family)
  mf <- match.call()
  mf$constraints <- substitute(getConstr(formula=formula, data=data, restrict=restrict))
  mf$family <- substitute(do.call(family, args=family.control))
  mf$family.control <- NULL
  mf$restrict <- NULL
  
  if(length(grep("s\\(",attr(terms(formula, data=data),"term.labels")))>0){
    mf[[1]] <- as.name("vgam")
    mod <- eval(mf)
  } else{
    mf[[1]] <- as.name("vglm")
    mod <- eval(mf)
  }
  return(mod)
}
