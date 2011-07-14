\name{questionnaire}
\alias{questionnaire}
\title{
Converting a choice experiment design into a choice experiment questionnaire
}
\description{
This function converts a choice experiment design created by the function \code{Lma.design} or \code{rotation.design} into choice experiment questions used in a questionnaire survey.
}
\usage{
questionnaire(choice.experiment.design, nblocks, nquestions, 
              nalternatives, nattributes)
}
\arguments{
  \item{choice.experiment.design}{
A data frame containing a choice experiment design (excluding an opt-out alternative) created by the function \code{Lma.design} or \code{rotation.design}.
}
  \item{nblocks}{
An integer value describing the number of blocks into which a choice experiment design is divided.
}
  \item{nquestions}{
An integer value describing the number of questions per block.
}
  \item{nalternatives}{
An integer value describing the number of alternatives per choice set (excluding an opt-out alternative).
}
  \item{nattributes}{
An integer value describing the number of attributes per alternative.
}
}
\details{
This function converts a choice experiment design created by the function \code{Lma.design} or \code{rotation.design} into choice experiment questions used in a questionnaire survey. The choice experiment design is assigned to the argument \code{choice.experiment.design}. The arguments \code{nblocks}, \code{nquestions}, \code{nalternatives}, and \code{nattributes} are respectively set according to the choice experiment design.
}
\value{
Choice experiment questions converted from the choice experiment design are returned in order of the values of the \code{nblocks} and \code{nquestions}.
}
\author{
Hideo Aizaki
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
questionnaire(choice.experiment.design = des1, nblocks = 1, 
 nquestions = 9, nalternatives = 2, nattributes = 3)

# A case of the function Lma.design
# See "Details" for the data set syn.res2.
des2 <- Lma.design(attribute.names = list(
 Eco = c("Conv.", "More", "Most"), 
 Price = c("1", "1.1", "1.2")), 
 nalternatives = 3, nblocks = 2, row.renames = FALSE, seed = 987)
questionnaire(choice.experiment.design = des2, nblocks = 2,
 nquestions = 9, nalternatives = 3, nattributes = 2)
}
\keyword{design}
