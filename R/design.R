##' Design Matrix
##' 
##' This function returns a data frame containing the design matrix for an ordinal
##' Bradley-Terry-Luce model.
##' 
##' @usage design(X, var1, var2, use.vars=NULL, reference=NULL, prefix="GAMMA", prefix.home="ALPHA", home.advantage=c("no","specific","yes"))
##' 
##' @param X a data frame in long format (see \code{\link{wide2long}}).
##' @param var1 a character specifying the column name from \code{X} 
##' that contains the first object that is compared.
##' @param var2 a character specifying the column name from \code{X} 
##' that contains the second object that is compared.
##' @param use.vars a character vector with the names of additional covariates 
##' from \code{X} to be included into the design matrix. 
##' (example: \code{use.vars = c("ENG", "SEX")}) if the variables \code{ENG} and \code{SEX} should be included. 
##' If all covariates in the data should be included, you can use \code{use.vars = "ALL"}. 
##' The default is \code{use.vars = NULL} for no additional covariates.
##' @param reference a character specifying the reference object. 
##' @param prefix a character that is used as prefix for the estimated object parameters 
##' @param prefix.home a character that is used as prefix for the estimated home advantage parameters 
##' @param home.advantage 
##'  \code{home.advantage="no"} uses no home advantage parameters,
##'  \code{home.advantage="specific"} uses one home advantage parameters for each object and
##'  \code{home.advantage="yes"} uses one home advantage parameter for all objects.
##' 
##' @return A data frame where each row represents a decision in a certain comparison for two objects.
##' 
##' @author Giuseppe Casalicchio
##' 
##' @example inst/examples/design_ex.R
##' @export 

design <-
  function(X, var1, var2, use.vars=NULL, reference=NULL,
           prefix="GAMMA", prefix.home="ALPHA",
           home.advantage=c("no","specific","yes")){
    dummy <-
      function(form, data, ...){
        dum <- dummyVars(form, data, ...)
        pred <- predict(dum, data)
        return(pred)
      }
    
    ### input 
    # X: dataset
    # var1: home Team
    # var2: guest Team
    # use.vars: character vector for additional (subject-specific) variables
    #           use "ALL" for all variables.
    # home.advantage: logical if home advantage should be considered
    ### output
    # Transformed dataset where 1: heimmannschaft, -1: gastmannschaft
    home.advantage <- match.arg(home.advantage)
    if(!is.null(use.vars)) {
      if(use.vars=="ALL") 
        append <- subset(X, select=-c(get(var1),get(var2))) else{
          append <- data.frame(X[, use.vars])
          colnames(append) <- use.vars
        }
    } else append <- NULL
    if(!inherits(X[, var1], "factor") | 
         !inherits(X[, var2], "factor") | 
         length(unique(X[, var1]))!=length(levels(X[, var1])) | 
         length(unique(X[, var2]))!=length(levels(X[, var2]))){
      X[, var1] <- as.factor(as.character(X[, var1]))
      X[, var2] <- as.factor(as.character(X[, var2]))
    }  
    if(length(reference)>1) stop("you can set only one reference object")
    
    missA <- levels(X[,var2])[!levels(X[,var2])%in%levels(X[,var1])]
    missB <- levels(X[,var1])[!levels(X[,var1])%in%levels(X[,var2])]
    
    levels(X[,var1]) <- c(levels(X[,var1]),missA)
    levels(X[,var2]) <- c(levels(X[,var2]),missB)
    
    indA <- dummy(as.formula(paste("~",var1)), X, levelsOnly=TRUE)
    indB <- dummy(as.formula(paste("~",var2)), X, levelsOnly=TRUE)
    
    level <- levels(X[,var1]) #unique(c(as.character(X[,var1]), as.character(X[,var2])))
    ret <- indA[,level]-indB[,level]
    colnames(ret) <- #gsub(" ",".",gsub("|[[:punct:]]", "", colnames(ret)))
      paste(prefix,gsub(" ",".",gsub("|[[:punct:]]", "", colnames(ret))), sep=".")
    
    if(home.advantage=="specific"){
      colnames(indA) <- 
        paste(prefix.home,gsub(" ",".",gsub("|[[:punct:]]", "", colnames(indA))), sep=".")
      if(is.null(use.vars)) append <- indA else append <- cbind(indA,append)
    }
    if(home.advantage=="yes"){
      if(!is.null(use.vars)) append$ALPHA <- 1 else {
        append <- data.frame(rep(1, nrow(ret)))
        colnames(append) <- prefix.home
      }
    }
    if(!is.null(reference)){
      if(grepl(prefix, reference)){
        ret <- ret[,!grepl(reference,colnames(ret))]
      } else{
        ret <- ret[,!grepl(paste(prefix,reference,sep="."),colnames(ret))]
      }
      
    } 
    output <- cbind(ret,append)
    if(inherits(output, "data.frame")) return(output) else return(as.data.frame(output))
  }
