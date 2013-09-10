\name{ordBTL}
\alias{ordBTL}
\title{ordinal Bradley-Terry-Luce model (ordBTL)}
\usage{
  ordBTL(formula, data, family=c("cumulative","acat"),
    family.control = list(), restrict=NULL, ...)
}
\arguments{
  \item{formula}{a formula describing the model to be
  fitted.}

  \item{data}{a data frame containing the design matrix for
  the model (See also \code{\link{design}} to generate such
  an design matrix).}

  \item{family}{a character specifying which ordinal model
  is to be fitted. Can be either \code{"cumulative"} for
  the cumulative link model or \code{"acat"} for the
  adjacent categories model.}

  \item{family.control}{a list with arguments passed to the
  corresponding \code{family}, either
  \code{\link[VGAM]{cumulative}} for the cumulative link
  model or \code{\link[VGAM]{acat}} for the adjacent
  categories model.}

  \item{restrict}{a character vector specifying the
  variables from \code{formula} that should be fitted with
  a symmetry constraint.}

  \item{...}{further arguments for fitting function
  (currently either \code{\link[VGAM]{vglm}} or
  \code{\link[VGAM]{vgam}}).}
}
\value{
  An object of class \item{\code{vglm}}{if no smoothing
  spline is used in the \code{formula} argument (see
  \code{\link[VGAM]{vglm-class}}).} \item{\code{vgam}}{if a
  smoothing spline with the function \code{\link[VGAM]{s}}
  is used in the \code{formula} argument (see
  \code{\link[VGAM]{vgam-class}}).}
}
\description{
  Fits ordinal regression models to paired comparison data.
}
\examples{
############################################################
##                                                        ##
## Example 1: Adjacent categories model with CEMS data    ##
##                                                        ##
############################################################

############################################################
# Reproduce results from Table 3 of Dittrich et al. (2001)
############################################################

# Get the CEMS data and generate design matrix
example(wide2long, package="ordBTL", echo=FALSE)
des1 <- design(CEMSlong, var1="object1", var2="object2", 
              use.vars="Y", reference="Stockholm")

# Fit the adjacent categories model, which corresponds to 
# the log-linear BTL model (see Agresti, 1992)
mod1 <- ordBTL(Y~., data=des1, family="acat", 
               family.control=list(reverse=TRUE))

# We get the same results from Table 3 of Dittrich et al (2001).
# Since Stockholm is the reference university, its estimate 
# is set to zero (due to identifiability)
coefs1 <- summary(mod1)@coef3
coefs1[order(coefs1[,"Estimate"], decreasing=TRUE),]

############################################################
# Reproduce results from Table 6 of Dittrich et al. (2001)
############################################################

# Generate design matrix and specify model formula
des2 <- design(CEMSlong, var1="object1", var2="object2", 
               use.vars="ALL", reference="Stockholm")
form2 <- Y~GAMMA.London + GAMMA.Paris + GAMMA.Milano + 
  GAMMA.StGallen + GAMMA.Barcelona + WOR +
  SEX + WOR:GAMMA.Paris + WOR:GAMMA.Milano +
  WOR:GAMMA.Barcelona + DEG:GAMMA.StGallen +
  STUD:GAMMA.Paris + STUD:GAMMA.StGallen +
  ENG:GAMMA.StGallen + FRA:GAMMA.London + 
  FRA:GAMMA.Paris + SPA:GAMMA.Barcelona +
  ITA:GAMMA.London + ITA:GAMMA.Milano +
  SEX:GAMMA.Milano

# Fit the adjacent categories model with symmetric 
# constraint for covariable WOR and SEX 
mod2 <- ordBTL(form2, data=des2, family="acat", 
               family.control=list(reverse=TRUE),
               restrict=c("WOR", "SEX"))

# We get the same results from Table 6 of Dittrich et al. (2001)
coefs2 <- summary(mod2)@coef3
coefs2[order(coefs2[,"Estimate"], decreasing=TRUE),]

# Notice that the change in sign for (Intercept), WOR and SEX
# is because we use here a different "coding".

############################################################
##                                                        ##
## Example 2: Various models from Agresti (1992)          ##
##                                                        ##
############################################################

# Data from Table 1 of Agresti (1992)
ribbon <- as.data.frame(matrix(c(4,4,0,5,5,8,4,
                                 5,12,4,6,0,2,1,
                                 0,2,2,2,5,15,4,
                                 4,4,5,4,2,8,3,
                                 6,9,3,4,1,4,3,
                                 2,4,1,6,4,8,5,
                                 5,5,6,6,1,4,3,
                                 0,1,0,3,1,13,12,
                                 1,6,3,4,3,11,2,
                                 6,12,5,3,1,3,0), ncol=7, byrow=TRUE))
ribbon$obj2 <- c(1,1,1,1,2,2,2,3,3,4)
ribbon$obj1 <- c(2,3,4,5,3,4,5,4,5,5)
head(ribbon)

# design matrix 
des3 <- design(ribbon, var1="obj1", var2="obj2", use.vars="ALL")

# Agresti (1992) uses the constraint that the object parameters 
# add up to 1. Since we constrain only one object parameter to be 
# equal to 0, we have to reparametrize the model formula as follows:
form3 <- cbind(V1,V2,V3,V4,V5,V6,V7)~I(GAMMA.1-GAMMA.5)+
  I(GAMMA.2-GAMMA.5)+I(GAMMA.3-GAMMA.5)+I(GAMMA.4-GAMMA.5)

# Fit an adjacent categories logit model
ac <- ordBTL(form3, data=des3, family="acat", 
              family.control=list(reverse=TRUE))

# Fit a cumulative logit model
clm.logit <- ordBTL(form3, data=des3)

# Fit a cumulative probit model
clm.probit <- ordBTL(form3, data=des3, family.control=list(link="probit"))

# Parameter estimates
coefs <- t(rbind("Adjacent categories logit"=coefficients(ac), 
                 "Cumulative probit"=coefficients(clm.probit),
                 "Cumulative logit"=coefficients(clm.logit)))
coefs <- rbind(coefs, "GAMMA.5"=0-colSums(coefs[4:7,]))
coefs
}
\author{
  Giuseppe Casalicchio
}
\references{
  Agresti A (1992). "Analysis of ordinal paired comparison
  data." _Applied Statistics_, pp. 287-297.

  Dittrich R, Hatzinger R and Katzenbeisser W (2001).
  "Corrigendum: Modelling the effect of subject-specific
  covariates in paired comparison studies with an
  application to university rankings." _Journal of the
  Royal Statistical Society: Series C (Applied
  Statistics)_, *50*(2), pp. 247-249.
}
\seealso{
  \code{\link[VGAM]{s}}, \code{\link[VGAM]{vgam}},
  \code{\link[VGAM]{vglm}}, \code{\link[ordBTL]{design}},
  \code{\link[VGAM]{plotvgam}}
}

