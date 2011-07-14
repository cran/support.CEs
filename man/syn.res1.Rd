\name{syn.res1}
\alias{syn.res1}
\docType{data}
\title{
Synthetic respondent data set (1)
}
\description{
A data set artificially created for the "Examples" in this package. 
}
\usage{data(syn.res1)}
\format{
  A data frame with 20 respondents on the following 12 variables.
  \describe{
    \item{\code{ID}}{Identification number of respondents}
    \item{\code{BLOCK}}{Serial number of blocks to which each respondent had been assigned}
    \item{\code{q1}}{Response to choice experiment question 1}
    \item{\code{q2}}{Response to choice experiment question 2}
    \item{\code{q3}}{Response to choice experiment question 3}
    \item{\code{q4}}{Response to choice experiment question 4}
    \item{\code{q5}}{Response to choice experiment question 5}
    \item{\code{q6}}{Response to choice experiment question 6}
    \item{\code{q7}}{Response to choice experiment question 7}
    \item{\code{q8}}{Response to choice experiment question 8}
    \item{\code{q9}}{Response to choice experiment question 9}
    \item{\code{F}}{Female dummy variable (\code{1} = Female, otherwise \code{0})}
  }
}
\details{
The "Examples" for each function in this package is based on two hypothetical cases in which choice experiments are applied to measure consumers' valuation of an agricultural product. 

The data set \code{syn.res1} is based on a case in which an unlabeled choice experiment design created by the mix-and-match method is used in a questionnaire survey. A total of 20 respondents were assumedly requested to select their most preferred from among two agricultural products and the option "none of these."

\tabular{c}{ \cr}
Q1. Please select your most preferred alternative from the following:

\tabular{lll}{
                 \tab Product 1 \tab Product 2 \cr
Region of origin \tab Region B \tab Region C \cr
Eco-friendly \tab More \tab Most \cr
Price \tab $1.1 \tab $1.2 \cr
}

\itemize{
\item{I select product 1.}
\item{I select product 2.}
\item{I select none of these.}
}

\tabular{c}{ \cr}
The agricultural product has three attributes: (1) the region of origin: this attribute has three levels---"Region A," "Region B," and "Region C."; (2) the eco-friendly label: this describe the three types of cultivation method---"Conv. (conventional cultivation method)," "More (more eco-friendly cultivation method)," and "Most (most eco-friendly cultivation method); and (3) the price per piece of the product---"$1," "$1.1," "$1.2." A total of 9 choice experiment questions are created by the function \code{rotation.design} . Each respondent had to respond to a total of 9 CE questions, implying that the sample size of the analysis based on their responses was 180 (= 9 CE questions per respondent * 20 respondents). In the example, the effect of the respondents' gender on their valuations of the cultivation methods was also examined (see "Examples" for the function \code{make.dataset}).
}
\author{
Hideo Aizaki
}
\seealso{
 \code{\link{make.dataset}}, \code{\link{rotation.design}}
}
\examples{
data(syn.res1)
syn.res1
}
\keyword{datasets}
