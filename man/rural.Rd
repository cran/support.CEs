\name{rural}
\alias{rural}
\docType{data}
\title{
Synthetic respondent data set: residents' valuation of rural environment conservation plan
}
\description{
Data set artificially created for an example based on a BDCE design. This example illustrates residents' valuation of rural environment conservation plan.}
\usage{
data(rural)
}
\format{
  Data frames with 400 respondents on the following 7 variables.
  \describe{
    \item{\code{ID}}{Identification number of respondents.}
    \item{\code{BLOCK}}{Serial number of blocks to which each respondent had been assigned.}
    \item{\code{q1}}{Response to choice experiment question 1.}
    \item{\code{q2}}{Response to choice experiment question 2.}
    \item{\code{q3}}{Response to choice experiment question 3.}
    \item{\code{q4}}{Response to choice experiment question 4.}
    \item{\code{Region}}{Region variable denoting whether the respondent was sampled from region 1 (\code{Region = 1}) or region 2 (\code{Region = 2}).}
  }
}
\author{
Hideo Aizaki
}
\seealso{
 \code{\link{make.dataset}}, \code{\link{make.design.matrix}}, \code{\link{Lma.design}}, \code{\link[stats]{glm}}
}
\examples{
library(stats)

if(getRversion() >= "3.6.0") RNGkind(sample.kind = "Rounding")

d.rural <- Lma.design(
  attribute.names = list(
    Area = c("20", "40", "60", "80"),
    Facility = c("None", "Agr", "Env", "Rec"),
    Tax = c("1000", "3000", "5000", "7000")),
  nalternatives = 1,
  nblocks = 4,
  row.renames = FALSE,
  seed = 987)

common.alt <- c(Area = "0", Facility = "None", Tax = "0")

dm.rural <- make.design.matrix(
  choice.experiment.design = d.rural,
  optout = FALSE,
  categorical.attributes = c("Facility"),
  continuous.attributes = c("Area", "Tax"),
  unlabeled = TRUE,
  common = common.alt,
  binary = TRUE)

data(rural)
rural1 <- subset(rural, Region == 1)
rural2 <- subset(rural, Region == 2)

ds.rural1 <- make.dataset(
  respondent.dataset = rural1,
  choice.indicators =
    c("q1", "q2", "q3", "q4"),
  design.matrix = dm.rural,
  detail = FALSE)

ds.rural2 <- make.dataset(
  respondent.dataset = rural2,
  choice.indicators =
    c("q1", "q2", "q3", "q4"),
  design.matrix = dm.rural,
  detail = FALSE)

fm.rural <- RES ~ Agr + Env + Rec + Area + Tax

out.rural1 <- glm(fm.rural,
                  family = binomial(link = "logit"),
                  data = ds.rural1)
summary(out.rural1)

out.rural2 <- glm(fm.rural,
                  family = binomial(link = "logit"),
                  data = ds.rural2)
summary(out.rural2)
}
\keyword{datasets}
