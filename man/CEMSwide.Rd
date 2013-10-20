\name{CEMSwide}
\alias{CEMSwide}
\docType{data}
\title{
CEMS Data
}
\description{
The Community of European management schools (CEMS) data used in the paper by Dittrich et al. (1998, 2001),
where the responses (the first 15 variables) are coded as
1 if the first university was preferred,  
2 if no decision was made and
3 if the second university was preferred.
}
\usage{data(CEMSwide)}
\format{
  A data frame with 303 observations on the following 23 variables.
  \describe{
    \item{\code{V1}}{London vs. Paris}
    \item{\code{V2}}{London vs. Milano}
    \item{\code{V3}}{Paris vs. Milano}
    \item{\code{V4}}{London vs. St.Gallen}
    \item{\code{V5}}{Paris vs. St.Gallen}
    \item{\code{V6}}{Milano vs. St.Gallen}
    \item{\code{V7}}{London vs. Barcelona}
    \item{\code{V8}}{Paris vs. Barcelona}
    \item{\code{V9}}{Milano vs. Barcelona}
    \item{\code{V10}}{St.Gallen vs. Barcelona}
    \item{\code{V11}}{London vs. Stockholm}
    \item{\code{V12}}{Paris vs. Stockholm}
    \item{\code{V13}}{Milano vs. Stockholm}
    \item{\code{V14}}{St.Gallen vs. Stockholm}
    \item{\code{V15}}{Barcelona vs. Stockholm}
    \item{\code{STUD}}{Main discipline of study : 2= commerce, 1= other (economics, business administration, business education)}
    \item{\code{ENG}}{Knowledge of English : 1= good, 2= poor}
    \item{\code{FRA}}{Knowledge of French : 1= good, 2= poor}
    \item{\code{SPA}}{Knowledge of Spanish : 1= good, 2= poor}
    \item{\code{ITA}}{Knowledge of Italian : 1= good, 2= poor}
    \item{\code{WOR}}{Full-time employment while studying: 1= no, 2= yes}
    \item{\code{DEG}}{Intention to take an international degree: 1= no, 2= yes}
    \item{\code{SEX}}{Gender : 1= female, 2= male}
  }
}
\source{
The Royal Statistical Society Datasets Website (\url{http://www.blackwellpublishing.com/rss/Readmefiles/dittrich.htm})}
\references{
  Dittrich R, Hatzinger R and Katzenbeisser W (1998).
  "Modelling the effect of subject-specific
  covariates in paired comparison studies with an
  application to university rankings." _Applied Statistics_, *47*(2), pp. 511-525.

  Dittrich R, Hatzinger R and Katzenbeisser W (2001).
  "Corrigendum: Modelling the effect of subject-specific
  covariates in paired comparison studies with an
  application to university rankings." _Journal of the
  Royal Statistical Society: Series C (Applied
  Statistics)_, *50*(2), pp. 247-249.
}
\examples{
data(CEMSwide)
## maybe str(CEMSwide) ; plot(CEMSwide) ...
}
\keyword{datasets}
