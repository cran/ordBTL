##' Ranking based on Estimates
##' 
##' Extracts the estimated parameters and sorts them based on their estimated values
##' 
##' @usage getRank(ordBTL, decreasing=TRUE, prefix=NULL, reference=NULL)
##' 
##' @param ordBTL a fitted model returned by \code{ordBTL}.
##' @param decreasing logical. Should the sort be increasing or decreasing? 
##' @param prefix (optional) a character that is included in the names of the parameters; only the parameters are returned that include this character (\code{prefix=NULL} extracts all estimated parameters).
##' @param reference (optional) a character specifying the reference object.
##' 
##' @return matrix containing the parameter estimates.
##' 
##' @seealso
##' \code{\link{ordBTL}}
##' 
##' @author Giuseppe Casalicchio
##' 
##' @example inst/examples/getRank_ex.R
##' @export 

getRank <- function(ordBTL, decreasing=TRUE, prefix=NULL, reference=NULL){
  if(!is.null(prefix)) 
    coefs <- summaryvglm(ordBTL)@coef3[grep(prefix, row.names(summaryvglm(ordBTL)@coef3)),, drop=FALSE] else
      coefs <- summaryvglm(ordBTL)@coef3
  
  if(!is.null(reference)){
    if(any(grepl(reference, rownames(coefs)))) 
      warning("Isn't '", reference, "' already included?")
    coefs <- rbind(coefs, c(0, NA, NA))
    rownames(coefs)[nrow(coefs)] <- reference
  } 
  rank <- coefs[order(coefs[,"Estimate"], decreasing=decreasing),, drop=FALSE]
  return(rank)
}
