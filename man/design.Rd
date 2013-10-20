\name{design}
\alias{design}
\title{Design Matrix}
\usage{
  design(X, var1, var2, use.vars=NULL, reference=NULL,
    prefix="GAMMA", prefix.home="ALPHA",
    home.advantage=c("no","specific","yes"))
}
\arguments{
  \item{X}{a data frame in long format (see
  \code{\link{wide2long}}).}

  \item{var1}{a character of the column name from \code{X}
  specifying the first object to be compared (or, in sport
  context, the home team).}

  \item{var2}{a character of the column name from \code{X}
  specifying the second object to be compared (or, in sport
  context, the away team).}

  \item{use.vars}{a character vector with the names of
  additional covariates of \code{X} that will additionally
  be included into the design matrix. (example:
  \code{use.vars = c("ENG", "SEX")}) if the covariates
  \code{ENG} and \code{SEX} should be included. If all
  covariates of \code{X} should be included, you can use
  \code{use.vars = "ALL"}. The default is \code{use.vars =
  NULL} for no additional covariates.}

  \item{reference}{a character specifying the reference
  object.}

  \item{prefix}{(optional) a character added in the names
  of the estimated object parameters}

  \item{prefix.home}{(optional) a character added in the
  names of the estimated home advantage parameters}

  \item{home.advantage}{Note that the home advantage is
  equivalent to an order effect \code{home.advantage="no"}
  uses no home advantage (order effect),
  \code{home.advantage="specific"} uses one home advantage
  (order effect) for each object and
  \code{home.advantage="yes"} uses one home advantage
  (order effect) for any object.}
}
\value{
  A data frame where each row refers to a pair comparison
  and each column corresponds to an object.
}
\description{
  This function returns the design matrix for an ordinal
  Bradley-Terry-Luce model.
}
\examples{
# load german football-league (Bundesliga) data
library("wikibooks")
data(Bundesliga)

# add new variable Y3 reflecting the response which is coded as 
# 1 if the home team wins
# 2 if the game ends up with a tie
# 3 if the home team loses
diff <- Bundesliga$Tore.Heim - Bundesliga$Tore.Gast
Bundesliga$Y3 <- as.ordered(ifelse(diff >= 1, 1, 
                                   ifelse(diff <= -1, 3, 2)))
buli0506 <- subset(Bundesliga, Saison=="2005/2006")
str(buli0506)

# Design matrix without home advantage
des.nohome <- design(buli0506, var1="Heim", var2="Gast", 
                     home.advantage="no")
str(des.nohome)

# Design matrix with one home advantage parameter for all objects
des.onehome <- design(buli0506, var1="Heim", var2="Gast", 
                      home.advantage="yes")
str(des.onehome)

# Design matrix with home advantage parameters for each object
des.teamhome <- design(buli0506, var1="Heim", var2="Gast",
                       home.advantage="specific")
str(des.teamhome)

# Design matrix with additional covariable "Spieltag"
des.covs <- design(buli0506, var1="Heim", var2="Gast", 
                   use.vars=c("Spieltag"), home.advantage="no")
str(des.covs)
}
\author{
  Giuseppe Casalicchio
}

