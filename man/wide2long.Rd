\name{wide2long}
\alias{wide2long}
\title{Reshapes Paired Comparison Data}
\usage{
  wide2long(data, paircomp, names=NULL, ...)
}
\arguments{
  \item{data}{a data frame in 'wide' format.}

  \item{paircomp}{a character vector specifying the columns
  from \code{data} that corresponds to a pair comparison.}

  \item{names}{a character vector of the same length as
  \code{paircomp} specifying the names (separated with a
  space) of the two objects that are compared in a pair
  comparison.}

  \item{...}{arguments to be passed to
  \code{\link[stats]{reshape}}.}
}
\value{
  The reshaped data frame.
}
\description{
  This function reshapes a data frame that contains pair
  comparison data in 'wide' format into a data frame in
  'long' format (see 'Details').
}
\details{
  In the 'wide' format each row reflects a certain
  subject/judge and a column contains for example the
  results of a pair comparison or is a subject-specific
  covariate. In the 'long' format each row represents a
  single pair comparison.
}
\examples{
data(CEMSwide)

# Define variable names containing the outcome of the comparisons
columns <- colnames(CEMSwide)[grep("V",colnames(CEMSwide))]

# Define names of the objects that are compared (shoud be separated with space)
# "London Paris" means that the first object is "London" and the second "Paris".
comparison <- c("London Paris", "London Milano", "Paris Milano", 
                "London St.Gallen", "Paris St.Gallen", 
                "Milano St.Gallen", "London Barcelona",
                "Paris Barcelona", "Milano Barcelona", 
                "St.Gallen Barcelona", "London Stockholm", 
                "Paris Stockholm", "Milano Stockholm",
                "St.Gallen Stockholm", "Barcelona Stockholm")

# Transform to 'long' format where v.names="Y" sets the name of 
# our response variable (see ?reshape)
CEMSlong <- wide2long(data=CEMSwide, paircomp=columns, names=comparison, v.names="Y")

head(CEMSlong)
head(CEMSwide)
}
\author{
  Giuseppe Casalicchio
}
\seealso{
  \code{\link[stats]{reshape}}, \code{\link{design}}
}

