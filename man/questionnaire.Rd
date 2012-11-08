\name{questionnaire}
\alias{questionnaire}
\title{
Converting a choice experiment design into a choice experiment questionnaire
}
\description{
This function converts a choice experiment design created by the function \code{Lma.design} or \code{rotation.design} into choice experiment questions used in a questionnaire survey.
}
\usage{
questionnaire(choice.experiment.design,
              common = NULL,
              quote = TRUE)
}
\arguments{
  \item{choice.experiment.design}{
A data frame containing a choice experiment design created by the function \code{Lma.design} or \code{rotation.design}.
}
  \item{common}{
A vector containing a fixed combination of attribute-levels corresponding to a common base option in each question. If there is no common base option, the argument is set as \code{NULL} (default).
}
  \item{quote}{
A logical variable indicating whether or not the attribute-levels in each question are printed wth quotation marks.
}
}
\details{
This function converts a choice experiment design created by the function \code{Lma.design} or \code{rotation.design} into choice experiment questions used in a questionnaire survey.
}
\value{
Choice experiment questions converted from the choice experiment design are returned.
}
\author{
Hideo Aizaki
}
\references{
Aizaki, H. (2012) Basic Functions for Supporting an Implementation of Choice Experiments in R. \emph{Journal of Statistical Software, Code Snippets}, \bold{50}(2), 1--24. \url{http://www.jstatsoft.org/v50/c02/}
}
\seealso{
 \code{\link{Lma.design}}, \code{\link{rotation.design}}
}
\examples{
# See "Examples" for the function make.dataset.
}
\keyword{design}
