\name{questionnaire}
\alias{questionnaire}
\title{
Converting a choice experiment design into a choice experiment questionnaire
}
\description{
This function converts a choice experiment design created by the function \code{Lma.design} or \code{rotation.design} into choice experiment questions used in a questionnaire survey.
}
\usage{
questionnaire(choice.experiment.design)
}
\arguments{
  \item{choice.experiment.design}{
A data frame containing a choice experiment design (excluding an opt-out alternative) created by the function \code{Lma.design} or \code{rotation.design}.
}
}
\details{
This function converts a choice experiment design created by the function \code{Lma.design} or \code{rotation.design} into choice experiment questions used in a questionnaire survey. The choice experiment design is assigned to the argument \code{choice.experiment.design}.
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
 \code{\link{Lma.design}}, \code{\link{rotation.design}}, \code{\link{syn.res1}}, \code{\link{syn.res2}}
}
\examples{
# A case of the function rotaion.design
# See "Details" for the data set syn.res1.
des1 <- rotation.design(attribute.names = list(
 Region = c("Reg_A", "Reg_B", "Reg_C"), 
 Eco = c("Conv.", "More", "Most"), 
 Price = c("1", "1.1", "1.2")), 
 nalternatives = 2, nblocks = 1, row.renames = FALSE, 
 randomize = TRUE, seed = 987)
questionnaire(choice.experiment.design = des1)

# A case of the function Lma.design
# See "Details" for the data set syn.res2.
des2 <- Lma.design(attribute.names = list(
 Eco = c("Conv.", "More", "Most"), 
 Price = c("1", "1.1", "1.2")), 
 nalternatives = 3, nblocks = 2, row.renames = FALSE, seed = 987)
questionnaire(choice.experiment.design = des2)
}
\keyword{design}
