\name{support.CEs-package}
\alias{support.CEs-package}
\alias{support.CEs}
\docType{package}
\title{
Basic functions for supporting an implementation of choice experiments
}
\description{
The package \pkg{support.CEs} provides seven basic functions that support an implementation of choice experiments. These include the following functions: two for creating a choice experiment design, which is based on orthogonal main-effect arrays; one for converting a choice experiment design into questionnaire format; one for converting a choice experiment design into a design matrix; one for making the data set suitable for a conditional logit model analysis using the function \code{clogit} in the package \pkg{survival}, or for a binary choice model analysis using the function \code{glm} in the package \pkg{stats}; one for calculating goodness-of-fit measures for an estimated model; and one for calculating the marginal willingness to pay for the attributes and/or levels of the estimated model. Version 0.3-0 and later versions of this package are also available for binary choice experiments. Refer to Aizaki (2012) and Aizaki et al. (2014) for details on the package.
}
\details{
The terms in this manual are defined as follows. An "attribute" is a characteristic or feature of an alternative. A "level" or "attribute level" represents the value of an attribute. One attribute can have two or more levels. An "alternative" is a combination of attributes; that is, one alternative can have two or more attributes. For example, when applying choice experiments to marketing research, the alternatives would refer to the "goods" or "services" that respondents are asked to select. A "choice set" refers to a set of alternatives available to individuals. One choice set includes two or more alternatives, including an opt-out alternative, if one exists. In a choice experiment question, respondents are usually asked to select the most preferred alternative from a choice set; therefore, one choice set constitutes a choice experiment question. A "choice experiment design" refers to a collection of individual choice sets.

The following shows an example of a choice experiment design. The choice experiment design includes a total of 9 choice sets (Q1 to Q9). Each choice set (question) consists of three alternatives ("Alternative 1," "Alternative 2," and "None of these" option). "Alternative 1" and "Alternative 2" each consist of three attributes: an attribute A with the three levels of "a1," "a2," and "a3"; an attribute B with the three levels of "b1," "b2," and "b3"; and an attribute C with the three levels of "c1," "c2," and "c3." 

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

\tabular{c}{ \cr}

Although there are three alternatives in the example above, the case of two alternatives for each question also exists. Such a choice experiment is known as a binary choice experiment.
}
\author{
Hideo Aizaki
}
\references{
Aizaki, H. (2012) Basic Functions for Supporting an Implementation of Choice Experiments in R. \emph{Journal of Statistical Software}, \bold{50}(C2), 1--24. \url{https://www.jstatsoft.org/v50/c02/}.

Aizaki, H. and Nishimura, K. (2008) Design and Analysis of Choice Experiments Using R: A Brief Introduction. \emph{Agricultural Information Research}, \bold{17}(2), 86--94. \url{https://www.jstage.jst.go.jp/article/air/17/2/17_2_86/_article}.

Aizaki, H., Nakatani, T. and Sato K. (2014) \emph{Stated Preference Methods Using R}. CRC Press.

Ben-Akiva, M. and Lerman, S. R. (1985) \emph{Discrete Choice Analysis: Theory and Application to Travel Demand}. The MIT Press.

Cameron, A. C. and Trivedi, P. K. (2005) \emph{Microeconometrics: Methods and Applications}. Cambridge University Press.

Hole, A. R. (2007) A Comparison of Approaches to Estimating Confidence Intervals for Willingness to Pay Measures. \emph{Health Economics}, \bold{16}, 827--840.

Johnson, F. R., Kanninen, B., Bingham, M. and Ozdemir, S. (2007) Experimental Design for Stated Choice Studies. In B. J. Kanninen (ed), \emph{Valuing Environmental Amenities Using Stated Choice Studies: A Common Sense Approach to Theory and Practice}. pp.159--202. Springer. 

Krinsky, I. and Robb. A. L. (1986) On Approximating the Statistical Properties of Elasticities. \emph{The Review of Economics and Statistics}, \bold{68}, 715--719.

Louviere, J. J., Hensher, D. A. and Swait, J. D. (2000) \emph{Stated Choice Methods: Analysis and Application}. Cambridge University Press.
}
\seealso{
  \code{\link[DoE.base]{oa.design}}, \code{\link[survival]{clogit}}, \code{\link[stats]{glm}}
}
\keyword{ package }

