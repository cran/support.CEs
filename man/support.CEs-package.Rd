\name{support.CEs-package}
\alias{support.CEs-package}
\alias{support.CEs}
\docType{package}
\title{
Basic functions for supporting an implementation of choice experiments
}
\description{
The package \pkg{support.CEs} provides seven basic functions that support an implementation of choice experiments. These include the following functions: two for creating a choice experiment design, which is based on orthogonal main-effect arrays; one for converting a choice experiment design into questionnaire format; one for converting a choice experiment design into a design matrix; one for making the data set suitable for the implementation of the function \code{clogit} in the package \pkg{survival}; one for calculating goodness-of-fit measures for an estimated model; and one for calculating the marginal willingness to pay for the attributes and/or levels of the estimated model.
}
\details{
\tabular{ll}{
Package: \tab support.CEs\cr
Type: \tab Package\cr
Version: \tab 0.2-2\cr
Date: \tab 2012-05-28\cr
License: \tab GPL (>=2)\cr
LazyLoad: \tab yes\cr
}
The terms in this manual are defined as follows. An "attribute" is a characteristic or feature of an alternative. A "level" or "attribute level" represents the value of an attribute. One attribute can have two or more levels. An "alternative" is a combination of attributes; that is, one alternative can have two or more attributes. For example, when applying choice experiments to marketing research, the alternatives would refer to the "goods" or "services" that respondents are asked to select. A "choice set" refers to a set of alternatives available to individuals. One choice set includes two or more alternatives, including an opt-out alternative, if one exists. In a choice experiment question, respondents are usually asked to select the most preferred alternative from a choice set; therefore, one choice set constitutes a choice experiment question. A "choice experiment design" refers to a collection of individual choice sets.

The following shows an example of a choice experiment design. The choice experiment design includes a total of 9 choice sets (Q1 to Q9). Each choice set (question) consists of three alternatives ("Alternative 1," "Alternative 2," and "None of these" option). "Alternative 1" and "Alternative 2" each consist of three attributes: an attribute A with the three levels of "a1," "a2," and "a3"; an attribute B with the three levels of "b1," "b2, and "b3"; and an attribute C with the three levels of "c1," "c2," and "c3." 

\tabular{c}{ \cr}

Q1. Please select your most preferred alternative from the following:

\tabular{lcc}{
            \tab Alternative 1 \tab Alternative 2 \cr
Attribute A \tab a2 \tab a3 \cr
Attribute B \tab b2 \tab b3 \cr
Attribute C \tab c2 \tab c3 \cr
}
\itemize{
\item{I select alternative 1.}
\item{I select alternative 2.}
\item{I select none of these.}
}

\tabular{c}{ \cr}

Q2. Please select your most preferred alternative from the following:

\tabular{lcc}{
            \tab Alternative 1 \tab Alternative 2 \cr
Attribute A \tab a2 \tab a1 \cr
Attribute B \tab b3 \tab b2 \cr
Attribute C \tab c1 \tab c3 \cr
}
\itemize{
\item{I select alternative 1.}
\item{I select alternative 2.}
\item{I select none of these.}
}

\tabular{c}{
<< Q3 to Q8 are omitted for the sake of simplicity>> \cr
}

Q9. Please select your most preferred alternative from the following:

\tabular{lcc}{
            \tab Alternative 1 \tab Alternative 2 \cr
Attribute A \tab a3 \tab a2 \cr
Attribute B \tab b2 \tab b2 \cr
Attribute C \tab c1 \tab c2 \cr
}
\itemize{
\item{I select alternative 1.}
\item{I select alternative 2.}
\item{I select none of these.}
}

}
\author{
Hideo Aizaki

Maintainer: Hideo Aizaki <aizaki@affrc.go.jp>
}
\seealso{
  \code{\link[DoE.base]{oa.design}}, \code{\link[survival]{clogit}}
}
\keyword{ package }

