\name{ribbon}
\alias{ribbon}
\docType{data}
\title{
Typewriter Ribbon Data
}
\description{
The typwriter ribbon data used in the paper by Agresti (1992), where 5 different typewriter ribbon brands were compared pairwise by 30 secretaries
}
\usage{data(ribbon)}
\format{
  A data frame with columns:
  \describe{
    \item{\code{V1}}{absolute frequency reflecting strong preference for corresponding \code{obj1}}
    \item{\code{V2}}{absolute frequency reflecting moderate preference for corresponding \code{obj1}}
    \item{\code{V3}}{absolute frequency reflecting weak preference for corresponding \code{obj1}}
    \item{\code{V4}}{absolute frequency reflecting no preference}
    \item{\code{V5}}{absolute frequency reflecting weak preference for corresponding \code{obj2}}
    \item{\code{V6}}{absolute frequency reflecting moderate preference for corresponding \code{obj2}}
    \item{\code{V7}}{absolute frequency reflecting strong preference for corresponding \code{obj2}}
    \item{\code{obj1}}{integer 1, ..., 5 referring to the 5 ribbon brands}
    \item{\code{obj2}}{integer 1, ..., 5 referring to the 5 ribbon brands}
  }
}
\source{
  Agresti A (1992). "Analysis of ordinal paired comparison
  data." _Applied Statistics_, pp. 287-297. Table 1.
}
\references{
  Agresti A (1992). "Analysis of ordinal paired comparison
  data." _Applied Statistics_, pp. 287-297.
}
\examples{
data(ribbon)
## maybe str(ribbon) ; plot(ribbon) ...
}
\keyword{datasets}
