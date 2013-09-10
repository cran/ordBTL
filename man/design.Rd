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

  \item{var1}{a character specifying the column name from
  \code{X} that contains the first object that is
  compared.}

  \item{var2}{a character specifying the column name from
  \code{X} that contains the second object that is
  compared.}

  \item{use.vars}{a character vector with the names of
  additional covariates from \code{X} to be included into
  the design matrix. (example: \code{use.vars = c("ENG",
  "SEX")}) if the variables \code{ENG} and \code{SEX}
  should be included. If all covariates in the data should
  be included, you can use \code{use.vars = "ALL"}. The
  default is \code{use.vars = NULL} for no additional
  covariates.}

  \item{reference}{a character specifying the reference
  object.}

  \item{prefix}{a character that is used as prefix for the
  estimated object parameters}

  \item{prefix.home}{a character that is used as prefix for
  the estimated home advantage parameters}

  \item{home.advantage}{\code{home.advantage="no"} uses no
  home advantage parameters,
  \code{home.advantage="specific"} uses one home advantage
  parameters for each object and
  \code{home.advantage="yes"} uses one home advantage
  parameter for all objects.}
}
\value{
  A data frame where each row represents a decision in a
  certain comparison for two objects.
}
\description{
  This function returns a data frame containing the design
  matrix for an ordinal Bradley-Terry-Luce model.
}
\examples{
# load german football-league (Bundesliga) data
library("wikibooks")
data(Bundesliga)
?Bundesliga

# add new variable Y3 reflecting the response which is coded as 
# 1 if the home team wins
# 0 if the game ends up with a tie
# -1 if the home team loses
diff <- Bundesliga$Tore.Heim - Bundesliga$Tore.Gast
Bundesliga$Y3 <- as.ordered(ifelse(diff > 0, 1, 
                                   ifelse(diff < 0, -1, diff)))
buli0405 <- subset(Bundesliga, Saison=="2004/2005")
str(buli0405)

# Design matrix without home advantage
des <- design(buli0405, var1="Heim", var2="Gast", 
              home.advantage="no")
str(des)

# Design matrix with one home advantage parameter for all objects
des.all <- design(buli0405, var1="Heim", var2="Gast", 
                  home.advantage="yes")
str(des.all)

# Design matrix with home advantage parameters for each object
des.spe <- design(buli0405, var1="Heim", var2="Gast",
                  home.advantage="specific")
str(des.spe)

# Design matrix with additional covariable "Spieltag"
des.covs <- design(buli0405, var1="Heim", var2="Gast", 
                   use.vars=c("Spieltag"), home.advantage="no")
str(des.covs)
}
\author{
  Giuseppe Casalicchio
}

