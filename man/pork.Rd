\name{pork}
\alias{pork}
\docType{data}
\title{
Synthetic respondent data set: consumers' valuation of pork
}
\description{
Data set artificially created for an example based on a labeled DCE design. This example illustrates consumers' valuation of pork.}
\usage{
data(pork)
}
\format{
  Data frames with 200 respondents on the following 6 variables.
  \describe{
    \item{\code{ID}}{Identification number of respondents.}
    \item{\code{BLOCK}}{Serial number of blocks to which each respondent had been assigned.}
    \item{\code{q1}}{Response to choice experiment question 1.}
    \item{\code{q2}}{Response to choice experiment question 2.}
    \item{\code{q3}}{Response to choice experiment question 3.}
    \item{\code{q4}}{Response to choice experiment question 4.}
  }
}
\author{
Hideo Aizaki
}
\seealso{
 \code{\link{make.dataset}}, \code{\link{make.design.matrix}}, \code{\link{Lma.design}}, \code{\link[survival]{clogit}}
}
\examples{
library(survival)

d.pork <- Lma.design(
  attribute.names = list(
    Price = c("100", "130", "160", "190")),
  nalternatives = 3,
  nblocks = 4,
  row.renames = FALSE,
  seed = 987)

data(pork)

dm.pork <- make.design.matrix(
  choice.experiment.design = d.pork,
  optout = TRUE,
  continuous.attributes = c("Price"),
  unlabeled = FALSE)

ds.pork <- make.dataset(
  respondent.dataset = pork,
  choice.indicators =
    c("q1", "q2", "q3", "q4"),
  design.matrix = dm.pork)

fm.pork <- RES ~ ASC1 + Price1 +
                 ASC2 + Price2 +
                 ASC3 + Price3 +
                 strata(STR)
out.pork <- clogit(fm.pork, data = ds.pork)
out.pork
}
\keyword{datasets}
