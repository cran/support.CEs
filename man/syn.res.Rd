\name{syn.res1, syn.res2, syn.res3}
\alias{syn.res1}
\alias{syn.res2}
\alias{syn.res3}
\docType{data}
\title{
Synthetic respondent data sets
}
\description{
Data sets artificially created for the "Examples" in this package. 
}
\usage{
data(syn.res1)
data(syn.res2)
data(syn.res3)
}
\format{
  Data frames with 100 respondents on the following 11 or 12 variables.
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
    \item{\code{F}}{Female dummy variable (\code{1} = Female, otherwise \code{0}) that is included only in \code{syn.res1}.}
  }
}
\details{
The "Examples" for each function in this package is based on three hypothetical cases in which choice experiments are applied to measure consumers' valuation of an agricultural product. 

The agricultural product has three attributes: (1) the region of origin: this attribute has three levels---"Region A," "Region B," and "Region C."; (2) the eco-friendly label: this describe the three types of cultivation method---"Conv. (conventional cultivation method)," "More (more eco-friendly cultivation method)," and "Most (most eco-friendly cultivation method); and (3) the price per piece of the product---"$1," "$1.1," "$1.2." 

\itemize{

\item{\code{syn.res1} (Unlabeled choice experiments)}

The data set \code{syn.res1} is based on a case in which an unlabeled choice experiment design created by the mix-and-match method is used in a questionnaire survey. A total of 100 respondents were assumed to have been requested to select their most preferred from among two agricultural products and the option "none of these."

\tabular{c}{ 
Q1. Please select your most preferred alternative from the following:
}

\tabular{lll}{
                 \tab Product 1 \tab Product 2 \cr
Region of origin \tab Region B \tab Region C \cr
Eco-friendly \tab More \tab Most \cr
Price \tab $1.1 \tab $1.2
}
\tabular{ll}{ 
\tab 1. I select product 1. \cr
\tab 2. I select product 2. \cr
\tab 3. I select none of these. 
}


A total of 9 choice experiment questions are created by the function \code{rotation.design} . Each respondent had to respond to a total of 9 choice experiment questions, implying that the sample size of the analysis based on their responses was 900 (= 9 choice experiment questions per respondent * 100 respondents). In the example, the effect of the respondents' gender on their valuations of the cultivation methods was also examined (see "Examples" for the function \code{make.dataset}). See the first case in "Example" for the function \code{make.dataset}.

\item{\code{syn.res2} (Labeled choice experiments)}

The data set \code{syn.res2} is based on a case in which a labeled choice experiment design created by the \emph{L^MA} method is used in a questionnaire survey. A total of 100 respondents were assumed to have been requested to select their most preferred from among three agricultural products and the option "none of these."

\tabular{c}{
Q1. Please select your most preferred alternative from the following:
}

\tabular{llll}{
             \tab Region A \tab Region B \tab Region C \cr
Eco-friendly \tab Most \tab Conv. \tab More \cr
Price \tab $1.1 \tab $1 \tab $1.1
}

\tabular{ll}{
\tab 1. I select the Region A product. \cr
\tab 2. I select the Region B product. \cr
\tab 3. I select the Region C product. \cr
\tab 4. I select none of these. 
}

Although the agricultural products have also three attributes and their levels mentioned above, the region of origin attribute is treated as an alternative specific attribute: the first, second, and third alternatives in a choice set always read as "Region A," "Region B," and "Region C," respectively. 

A total of 18 choice experiment questions were created by the function \code{Lma.design} and divided into two blocks: this means that two types of questionnaire were created, each of which was randomly assigned to the respondents. Therefore, the sample size of the analysis based on their responses was 900 (= 9 choice experiment questions per respondent * 100 respondents). See the second case in "Examples" for the function \code{make.dataset}.

\item{\code{syn.res3} (Binary choice experiments with an opt-out option)}

The data set \code{syn.res3} is based on a case in which a binary choice experiment design created by the \emph{L^MA} method is used in a questionnaire survey. A total of 100 respondents were assumed to have been requested to decide whether they select an agricultural product or not.

\tabular{c}{ 
Q1. Would you like to purchase the following?
}

\tabular{ll}{
                 \tab Product \cr
Region of origin \tab Region A \cr
    Eco-friendly \tab Conv. \cr
           Price \tab $1 
}

\tabular{ll}{
\tab 1. Yes. \cr
\tab 2. No. 
}

A total of 9 binary choice experiment questions are created by the function \code{Lma.design}. Each respondent had to respond to a total of 9 binary choice experiment questions, implying that the sample size of the analysis based on their responses was 900 (= 9 binary choice experiment questions per respondent * 100 respondents). See the last case in "Examples" for the function \code{make.dataset}.
}
}
\author{
Hideo Aizaki
}
\references{
Aizaki, H. (2012) Basic Functions for Supporting an Implementation of Choice Experiments in R. \emph{Journal of Statistical Software, Code Snippets}, \bold{50}(2), 1--24. \url{http://www.jstatsoft.org/v50/c02/}
}
\seealso{
 \code{\link{make.dataset}}, \code{\link{rotation.design}}, \code{\link{Lma.design}}
}
\examples{
# See "Examples" for the function make.dataset.
}
\keyword{datasets}
