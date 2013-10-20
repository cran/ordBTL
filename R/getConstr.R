##' Function for internal usage
##' 
##' @keywords internal
##' 

getConstr <- function(formula, data, restrict=NULL){
#   if(!is.null(reference)) {
#     formula <- get.formula(formula, data, reference)
#   }
  getVars <-
    function(formula, data){
      attr(terms(formula, data=data),"term.labels")
    }
  response <- as.character(formula)[2]#all.vars(formula)[1]
  if(length(grep("cbind",response))!=0){
    nthresholds <- length(gregexpr(",", response)[[1]])
  } else{
    if(!inherits(data[,response], "ordered")){
      warning("response variable will be transformed to a ordinal factor---see ordered()")
      data[,response] <- as.ordered(data[,response])
    }
    nthresholds <- length(levels(data[,response]))-1
  }
  nointercept <- length(grep("-1",gsub(" ", "", as.character(formula))))>0
  if(nointercept){
    constrVars <- getVars(formula, data)
    #constr <- vector("list", length(constrVars))
    #names(constr) <- constrVars
    #for(i in 1:length(constrVars)) constr[[i]] <- matrix(rep(1, nthresholds))
    #return(constr)
  } else{
    constrVars <- getVars(formula, data)
    constrVars <- c("(Intercept)", constrVars)
  }

  constr <- vector("list", length(constrVars))
  names(constr) <- constrVars
  
  for(i in 1:length(constrVars)) constr[[i]] <- matrix(rep(1, nthresholds))
  
  if(nthresholds%%2==0){
    matr <- diag(nthresholds/2)
    if(!nointercept) constr[["(Intercept)"]] <- rbind(matr, apply(t(matr*-1),2,rev))
    if(!is.null(restrict)){
      restrict <- restrict[restrict%in%constrVars] # added 23.09.2013
      for(i in restrict) constr[[i]] <- rbind(matr, apply(t(matr*-1),2,rev))
    }
  } else{
    if(nthresholds==1){
      constr <- NULL #constr[["(Intercept)"]] <- matrix(1,ncol=1)
    } else{
    cols <- (nthresholds-1)/2
    matr1 <- diag(cols)
    matr2 <- diag(cols)*-1
    if(!nointercept) constr[["(Intercept)"]] <- rbind(matr1, rep(0,times=cols), apply(t(matr2),2,rev))
      if(!is.null(restrict)){
        restrict <- restrict[restrict%in%constrVars] # added 23.09.2013
        for(i in restrict) constr[[i]] <- 
          rbind(matr1, rep(0,times=cols), apply(t(matr2),2,rev))
      }
    }
  }
  return(constr)
}
