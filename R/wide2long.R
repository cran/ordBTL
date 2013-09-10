##' Reshapes Paired Comparison Data
##' 
##' This function reshapes a data frame that contains pair comparison data in 
##' 'wide' format into a data frame in 'long' format (see 'Details').
##' 
##' @usage wide2long(data, paircomp, names=NULL, ...)
##' 
##' @param data a data frame in 'wide' format.
##' @param paircomp a character vector specifying the columns from \code{data} 
##' that corresponds to a pair comparison.
##' @param names a character vector of the same length as \code{paircomp} specifying the names (separated with a space) of the two objects 
##' that are compared in a pair comparison. 
##' @param ... arguments to be passed to \code{\link[stats]{reshape}}.
##' 
##' @details 
##' In the 'wide' format each row reflects a certain subject/judge and a column contains for example the results of a pair comparison or is a subject-specific covariate.
##' In the 'long' format each row represents a single pair comparison. 
##' 
##' @return The reshaped data frame.
##' 
##' @seealso
##' \code{\link[stats]{reshape}}, 
##' \code{\link{design}}
##' 
##' @author Giuseppe Casalicchio
##' 
##' @example inst/examples/wide2long_ex.R
##' @export 

wide2long <-
function(data, paircomp, names=NULL, ...){
  # input:
  ## paircomp: character vector specifying the pair comparison columns from data
  ## names: character vector with object names (separated with space)
  ## ...
  # output:
  ## dataset long format
  if(is.null(names)) names <- paircomp
  if(length(grep(" ",names))!=length(names))
    stop("some 'names' do not contain space character")
  if(!any(paircomp%in%colnames(data)))
    stop("some 'paircomp' do not match with colnames from dataset")
  
  long <- reshape(data, varying=list(paircomp), direction="long", ...)
  playernames <- strsplit(unlist(lapply(names, rep, nrow(data))), " ")
  
  long$object1 <- as.factor(sapply(playernames, function(X){X[1]}))
  long$object2 <- as.factor(sapply(playernames, function(X){X[2]}))
  
  long$time <- NULL #long$id
  return(long)
}
