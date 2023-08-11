\name{ce.response}
\alias{ce.response}
\title{
Generating artificial responses to choice experiment questions
}
\description{
This function synthesizes responses to choice experiment questions.
}
\usage{
ce.response(design,
            categorical.attributes = NULL, 
            continuous.attributes = NULL,
            optout = FALSE,
            b,
            n,
            detail = FALSE,
            seed = NULL)
}
\arguments{
  \item{design}{
An object of the S3 class "cedes". 
}
  \item{categorical.attributes}{
A vector containing the names of attributes treated as categorical independent variables in the model. If there are no categorical variables, it is set as \code{NULL} (default).
}
  \item{continuous.attributes}{
A vector containing the names of attributes treated as continuous independent variables in the model.  If there are no continuous variables, it is set as \code{NULL} (default).
}
  \item{optout}{
A logical variable describing whether or not the opt-out alternative is included in the choice experiment questions. If \code{TRUE} (default), the opt-out alternative is included; otherwise it is not.
}
  \item{b}{
A vector containing parameters of independent variables in the model. The vector is used to calculate utilities for alternatives.
}
  \item{n}{
An integer value showing the number of respondents in the resultant dataset.
} 
  \item{detail}{
A logical variable: if \code{TRUE}, the dataset is returned in a detailed format; and if \code{FALSE} (default), the dataset is return in a simple format.
}
  \item{seed}{
Seed for a random number generator.
}
}
\details{
This function synthesizes responses to choice experiment questions. Before using the function, see help for functions \code{rotation.design}, \code{make.design.matrix}, and \code{make.dataset}. For details on the usage, refer to section Examples below.
}
\value{
This function returns a data frame that contains synthesized responses to choice experiment questions, in either a detailed or a simple format. The detailed format dataset contains the following variables, as well as independent variables according to the arguments \code{categorical.attributes} and/or \code{continuous.attributes}.
  \item{ID}{An identification number of artificial respondents.}
  \item{BLOCK}{A serial number of blocks in the choice sets.}
  \item{QES}{A serial number of questions.}
  \item{ALT}{A serial number of alternatives.}
  \item{ASC}{Alternative specific constant.}
  \item{STR}{A stratification variable used to identify each combination of respondent and question.}
  \item{RES}{Responses to choice experiment questions, taking the value of 1 if an alternative is selected by the synthesized respondents and 0 otherwise.}
}
\author{
Hideo Aizaki
}
\seealso{
\code{\link{rotation.design}}, \code{\link{make.design.matrix}}, \code{\link{make.dataset}}, \code{\link{rice}}, \code{\link[survival]{clogit}}
}
\examples{
# The following example is based on the dataset rice.
# The choice sets differ from those in the dataset rice.

library(survival)

dsgn.rice <- rotation.design(
  attribute.names = list(
    Region = c("RegA", "RegB", "RegC"),
    Cultivation = c("Conv", "NoChem", "Organic"),
    Price = c("1700", "2000", "2300")),
  nalternatives = 2,
  nblocks = 3,
  row.renames = FALSE,
  randomize = TRUE,
  seed = 987)

syn.rice1 <- ce.response(
  design = dsgn.rice, 
  categorical.attributes = c("Region", "Cultivation"),
  continuous.attributes = "Price",
  optout = TRUE,
  b = c(4.4, 0.47, 0.97, 0.66, 1.1, -0.0023),
  n = 99,
  detail = TRUE,
  seed = 1)
head(syn.rice1, 30)
mf <- RES ~ ASC + RegB + RegC + NoChem + Organic + Price + strata(STR)
(out1 <- clogit(mf, data = syn.rice1))

syn.rice.simple <- ce.response(
  design = dsgn.rice, 
  categorical.attributes = c("Region", "Cultivation"),
  continuous.attributes = "Price",
  optout = TRUE,
  b = c(4.4, 0.47, 0.97, 0.66, 1.1, -0.0023),
  n = 99,
  detail = FALSE,
  seed = 1)
head(syn.rice.simple)
dsgnmat.rice <- make.design.matrix(
  choice.experiment.design = dsgn.rice,
  optout = TRUE,
  categorical.attributes = c("Region", "Cultivation"),
  continuous.attributes = c("Price"),
  unlabeled = TRUE)
syn.rice2 <- make.dataset(
  respondent.dataset = syn.rice.simple,
  choice.indicators =
    c("q1", "q2", "q3"),
  design.matrix = dsgnmat.rice)
(out2 <- clogit(mf, data = syn.rice2))

all.equal(coef(out1), coef(out2))

}
\keyword{datagen}
\keyword{survival}
