\name{rice}
\alias{rice}
\docType{data}
\title{
Synthetic respondent data set: consumers' valuation of rice
}
\description{
Data set artificially created for an example based on an unlabeled DCE design. This example illustrates consumers' valuation of rice.}
\usage{
data(rice)
}
\format{
  Data frames with 100 respondents on the following 12 variables.
  \describe{
    \item{\code{ID}}{Identification number of respondents.}
    \item{\code{BLOCK}}{Serial number of blocks to which each respondent had been assigned.}
    \item{\code{q1}}{Response to choice experiment question 1.}
    \item{\code{q2}}{Response to choice experiment question 2.}
    \item{\code{q3}}{Response to choice experiment question 3.}
    \item{\code{q4}}{Response to choice experiment question 4.}
    \item{\code{q5}}{Response to choice experiment question 5.}
    \item{\code{q6}}{Response to choice experiment question 6.}
    \item{\code{q7}}{Response to choice experiment question 7.}
    \item{\code{q8}}{Response to choice experiment question 8.}
    \item{\code{q9}}{Response to choice experiment question 9.}
    \item{\code{F}}{Female dummy variable (\code{1} = Female, otherwise \code{0}).}
  }
}
\author{
Hideo Aizaki
}
\seealso{
 \code{\link{make.dataset}}, \code{\link{make.design.matrix}}, \code{\link{rotation.design}}, \code{\link[survival]{clogit}}
}
\examples{
library(survival)

d.rice <- rotation.design(
  attribute.names = list(
    Region = c("RegA", "RegB", "RegC"),
    Cultivation = c("Conv", "NoChem", "Organic"),
    Price = c("1700", "2000", "2300")),
  nalternatives = 2,
  nblocks = 1,
  row.renames = FALSE,
  randomize = TRUE,
  seed = 987)

data(rice)

dm.rice <- make.design.matrix(
  choice.experiment.design = d.rice,
  optout = TRUE,
  categorical.attributes = c("Region", "Cultivation"),
  continuous.attributes = c("Price"),
  unlabeled = TRUE)

ds.rice <- make.dataset(
  respondent.dataset = rice,
  choice.indicators =
    c("q1", "q2", "q3", "q4", "q5",
      "q6", "q7", "q8", "q9"),
  design.matrix = dm.rice)

fm.rice <- RES ~ ASC + RegB + RegC + NoChem + Organic +
           NoChem:F + Organic:F + Price + strata(STR)
out.rice <- clogit(fm.rice, data = ds.rice)
out.rice
}
\keyword{datasets}
