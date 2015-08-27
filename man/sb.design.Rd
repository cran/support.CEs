\name{sb.design}
\alias{sb.design}
\alias{print.sb}
\alias{summary.sb}
\alias{print.summary.sb}
\title{
Creating or checking a choice experiment design using the Street and Burgess Method
}
\description{
This function creates or checks a choice experiment design according to the method developed by Street, D. J. and Burgess, L. on the website \bold{Discrete Choice Experiments}.
}
\usage{
sb.design(operation = "construct", nattributes, nalternatives, nlevels,
          attribute.names, design = NULL, generators = NULL,
          effect = "main", interactions = NULL, determinant = NULL,
          nblocks = 1, seed = NULL, ...)

\method{print}{sb}(x, ...)

\method{summary}{sb}(object, ...)

\method{print}{summary.sb}(x, ...)
}
\arguments{
  \item{operation}{
A character variable describing the operation to be conducted: \code{"construct"} is used to construct a choice experiment design; \code{"check"} is used to check the choice experiment design that is assigned to the argument \code{design}.
}
  \item{nattributes}{
An integer value describing the number of attributes per alternative included in the choice sets. The range must be 2 <= \code{nattributes} <= 20.
}
  \item{nalternatives}{
An integer value describing the number of alternatives (i.e., options) per choice set included in the orthogonal main effect design (OMED) or choice sets, excluding an opt-out alternative or a common base alternative. The range must be 2 <= \code{nalternatives} <= 20.
}
  \item{nlevels}{
An integer vector describing number of levels for each attribute included in the choice sets. The values must be in the range [2, 20]. The order of the values must correspond to the order of attributes shown in an OMED or choice sets assigned to the argument \code{design}. For example, when \code{nlevels = c(2, 2, 3, 4)} is assigned to this argument, this means that the first and second attributes for the OMED/choice sets both have two levels, while the third and fourth attributes have three and four levels, respectively.
}
  \item{attribute.names}{
A list of the names of attributes and levels. Levels must be assigned as a character vector (see below). This argument is optional. The number of attributes included in this argument must be equal to the argument \code{nattributes}. The number of levels for each attribute must be equal to the values in the argument \code{nlevels}, and they must be in the same order. Consider the following example: attribute A has two levels, a0 and a1; attribute B has two levels, b0 and b1; attribute C has three levels, c0, c1, and c2; and attribute D has four levels, d0, d1, d2, and d3. These four attributes are assigned to the argument as follows:\cr
\code{attribute.names = list(A = c("a0", "a1"), B = c("b0", "b1"), } \cr
\code{C = c("c0", "c1", "c2"), D = c("d0", "d1", "d2", "d3"))}.\cr
In this case, the first, second, third, and fourth element of \code{nlevels} must show the number of levels for attributes A, B, C, and D, respectively:\cr 
\code{nlevels = c(2, 2, 3, 4)}.
}
  \item{design}{
A matrix describing an OMED corresponding to the first alternative for the argument \code{operation = "construct"} or choice sets for the argument \code{operation = "check"}. For an OMED with the argument \code{operation = "construct"}, each row corresponds to a treatment combination (i.e., an alternative), and each column corresponds to an attribute. For the choice sets with the argument \code{operation = "check"}, each row corresponds to a choice set, and each \code{nattributes} column corresponds to the attributes for each alternative. The first \code{nattributes} column correspond to the \code{nattributes} attributes for the first alternative, the second \code{nattributes} column corresponds to the \code{nattributes} attributes for the second alternative, and so on. For either the OMED or choice sets, the values in each column of design must be integers between 0 and (\code{nlevels} - 1), and they must correspond to the column. When generating an OMED with this function, the argument \code{design} is set to \code{NULL}. Thus, an OMED is generated according to the vector assigned to the argument \code{nlevels} using the function \code{oa.design()} in the \pkg{DoE.base} package.
}
  \item{generators}{
An integer vector or matrix describing the sets of generators for constructing the choice sets. Each row (or a vector) corresponds to one set of generators. One set of generators consists of (\code{nalternative} - 1) * \code{nattributes} elements. For example, assume the construction of choice sets that have three alternatives each, where each alternative has four attributes. A single set of generators thus consists of eight integer values (= (3 - 1) * 4). The first four elements correspond to the generator for constructing the second alternative in the choice sets. The remaining four elements correspond to the generator for constructing the third alternative in the choice sets. When only a single set of generators is selected, it is assigned to the argument \code{generators} as an integer vector. When two or more sets of generators are selected, they are assigned to the argument \code{generators} as an integer matrix. A set of generators with only elements of 0 are not permitted. For details regarding the formatting style of the generators, refer to section \bold{Examples}, below.
}
  \item{effect}{
A character variable describing the effect to be estimated: \code{"main"} is used for main effects; \code{"mplusall"} is used to estimate the main effects as well as all of the two-factor interactions; and \code{"mplussome"} is used to estimate the main effects along with only some two-factor interactions.
}
  \item{interactions}{
A vector or list of vectors describing the two-factor (i.e., two-attribute) interactions to be estimated. A single two-factor interaction is assigned as a vector with two elements. For example, \code{c(1, 2)} denotes the interaction between the first attribute and the second attribute. Two or more of these two-factor interactions are assigned as a list of two or more vectors with two elements each. For example, \code{list(c(1, 2), c(1, 3))} denotes two different two-factor interactions, one between the first attribute and the second attribute, and the other between the first attribute and the third attribute. The paired values must differ. The values must be in the range [1, \code{nattributes}]. This argument is valid only when the argument \code{effect} is set as \code{"mplussome"}.
}
  \item{determinant}{
A character variable describing the determinant of the information matrix. This argument is optional. The determinant must be in the range [0, 1]. The formatting style can be, e.g., \code{"17/12524124635136"} or \code{"1.35738e-12"}. Note that the determinant is not assigned as a numerical value (\code{17/12524124635136} or \code{1.35738e-12}), but rather as a character (\code{"17/12524124635136"} or \code{"1.35738e-12"}). In other words, it is important to enclose the determinant within double quotation marks.
}
  \item{nblocks}{
An integer value describing the number of blocks into which a choice experiment design is divided. This option is valid only when \code{operation ="construct"}. Note that one way to divide a design used in this function is simply to divide the design that is constructed by the website \bold{Discrete Choice Experiments} into \code{nblocks} subsets randomly. In that way, features of the design such as efficiency are not considered when dividing it.
}
  \item{seed}{
A seed for a random number generator.
}
  \item{\dots}{
Optional arguments.
}
  \item{x, object}{
Object of the S3 class \code{"sb"}.
}
}
\details{
The website \bold{Discrete Choice Experiments} (Burgess 2007) allows us to construct and check choice experiment designs according to the theory explained in Street and Burgess (2007). This function sends a request to the website to construct or check a choice experiment design from R. The calculations for constructing and checking are executed on the website. Thus, this function is merely an interface between your R and the website. When using this function, your R must be connected to the Internet.

The Street and Burgess Method constructs optimal choice experiment designs that are designed to estimate the main effects and two-factor interaction effects. The first alternative (attribute-levels are expressed as integer values) is derived from an orthogonal main effect design, and the j-th (j = 2, 3, ...) alternative is created by adding special combinations of integer values (i.e., generators) to the first alternative. Refer to Street and Burgess (2007) for more details.

Among the arguments in this function, the following are important: \code{operation}, \code{nattributes}, \code{nlevels}, \code{nalternatives}, \code{design}, \code{generators}, \code{effect}, \code{interactions}, and \code{determinant}. Explanations for these arguments above are based on the \bold{Help} page on website \bold{Discrete Choice Experiments} (Burgess 2007): please refer to the \bold{Help} page for more information regarding these arguments.

Note that a message \code{"Error in sb.check.args(..."} will appear on the R console after executing this function with incorrect settings for the arguments. In such a case, set the argument correctly according to the message.
}
\value{
This function returns an object of the S3 class \code{"sb"} inheriting from the S3 class \code{"cedes"}. This object is a list with \code{alternatives}, \code{candidate}, \code{design.information}, and \code{sb}. The components \code{alternatives}, \code{candidate}, and \code{design.information} are the same as those in the S3 class \code{"cedes"} (see \code{\link{Lma.design}} or \code{\link{rotation.design}} for details). When \code{operation = "check"}, each of \code{alternatives}, \code{candidate}, and \code{design.information} is \code{NULL}: the output from this function cannot be used in other functions (i.e., \code{\link{questionnaire}} and \code{\link{make.design.matrix}}) in the package \pkg{support.CEs} when \code{operation = "check"}.

The component \code{sb}, which is the output from the website \bold{Discrete Choice Experiments}, is a list with the following six components:

  \item{input }{
A list of objects that were entered: \code{operation}, \code{effect}, \code{design} (treatment combinations or choice sets), \code{generators}, \code{determinant}, and \code{interactions}.
}
  \item{message }{
A character string containing messages regarding the calculation on the website.
}
  \item{output }{
A list of the resulting matrices: the choice sets, the contrast matrix (B), the Lambda matrix, the information matrix (C), the inverse of C, and the correlation matrix.
}
  \item{calculation }{
A logical variable taking on \code{TRUE} when the calculation process on the website is success, \code{FALSE} otherwise.
}
  \item{html }{
A character string containing html content received from the website. The length of the character string is long.
}
  \item{version }{
A character string containing software version used for the calculation process on the website.
}

Note that the order of rows in the design contained in the component \code{alternatives} is different from that in \code{sb} when \code{nblocks} >= 2. This is because the random division of the design into \code{nblocks} subset is executed after the design is constructed on the website.

This function may return a list of objects including html content when the Street and Burgess Method is not be implemented successfully. The html content is received from the website \bold{Discrete Choice Experiments}. By investigating this content, it is possible to determine what caused the unsuccessful results.
}

\references{
Burgess, L. (2007) \emph{Discrete Choice Experiments [computer software]}, Department of Mathematical Sciences, University of Technology, Sydney, available at \url{http://maths.science.uts.edu.au/maths/wiki/SPExpts}

Street, D. J., and Burgess, L. (2007) \emph{The Construction of Optimal Stated Choice Experiments}. Hoboken, NJ: John Wiley & Sons.
}
\author{
Hideo Aizaki
}
\note{
When publishing results obtained using this function, please cite Burgess (2007), Street and Burgess (2007), and the package \pkg{support.CEs}. When creating and publishing new functions that access the website \bold{Discrete Choice Experiments} on the basis of this function, please inform Dr. Burgess about the publication. Dr. Burgess' contact information is provided on the website \bold{Discrete Choice Experiments}.
}
\section{Acknowledgement }{
I would like to thank Dr. Burgess for her kind support in developing this function.
}

\seealso{
\code{\link[DoE.base]{oa.design}}, \code{\link{Lma.design}}, \code{\link{rotation.design}}
}
\examples{
# The following lines of code reproduce five examples shown on 
# the website Discrete Choice Experiments.

# Check Main Ex. 1
\dontrun{
CheckMainEx1.sets <- matrix(c(
  0, 0, 0, 0, 3, 2, 2, 2,
  2, 2, 0, 2, 1, 1, 2, 1,
  3, 0, 0, 0, 2, 2, 2, 2,
  1, 1, 0, 1, 0, 0, 2, 0,
  1, 0, 2, 0, 0, 2, 1, 2,
  3, 2, 1, 0, 2, 1, 0, 2,
  2, 1, 0, 0, 1, 0, 2, 2,
  0, 0, 0, 2, 3, 2, 2, 1,
  3, 0, 0, 1, 2, 2, 2, 0,
  0, 2, 2, 1, 3, 1, 1, 0,
  1, 2, 0, 0, 0, 1, 2, 2,
  1, 0, 1, 2, 0, 2, 0, 1,
  2, 0, 2, 0, 1, 2, 1, 2,
  2, 0, 1, 1, 1, 2, 0, 0,
  3, 1, 2, 2, 2, 0, 1, 1,
  0, 1, 1, 0, 3, 0, 0, 2),
  nrow = 16, byrow = TRUE)

CheckMainEx1 <- sb.design(
  operation       = "check", 
  nattributes     = 4,
  nlevels         = c(4, 3, 3, 3),
  nalternatives   = 2,
  attribute.names = list(A = c("a0", "a1", "a2", "a3"),
                         B = c("b0", "b1", "b2"),
                         C = c("c0", "c1", "c2"),
                         D = c("d0", "d1", "d2")),
  design          = CheckMainEx1.sets,
  effect          = "main")

CheckMainEx1
summary(CheckMainEx1)}

# Construct Main Ex. 1
\dontrun{
ConstructMainEx1.treatment <- matrix(c(
  0, 0, 0, 0,
  2, 2, 0, 2,
  3, 0, 0, 0,
  1, 1, 0, 1,
  1, 0, 2, 0,
  3, 2, 1, 0,
  2, 1, 0, 0,
  0, 0, 0, 2,
  3, 0, 0, 1,
  0, 2, 2, 1,
  1, 2, 0, 0,
  1, 0, 1, 2,
  2, 0, 2, 0,
  2, 0, 1, 1,
  3, 1, 2, 2,
  0, 1, 1, 0),
  nrow = 16, byrow = TRUE)

ConstructMainEx1 <- sb.design(
  operation       = "construct", 
  nattributes     = 4,
  nlevels         = c(4, 3, 3, 3),
  nalternatives   = 2,
  attribute.names = list(A = c("a0", "a1", "a2", "a3"), 
                         B = c("b0", "b1", "b2"),
                         C = c("c0", "c1", "c2"),
                         D = c("d0", "d1", "d2")),
  design          = ConstructMainEx1.treatment,
  generators      = c(3, 2, 2, 2),
  effect          = "main")

ConstructMainEx1
summary(ConstructMainEx1)}

# Construct Main Ex. 2
\dontrun{
ConstructMainEx2.treatment <- matrix(c(
  0, 0,
  0, 1,
  1, 0,
  1, 1), 
  nrow = 4, byrow = TRUE)

ConstructMainEx2 <- sb.design(
  operation       = "construct", 
  nattributes     = 2,
  nlevels         = c(2, 2),
  nalternatives   = 2,
  attribute.names = list(X = c("x0", "x1"), 
                         Y = c("y0", "y1")),
  design          = ConstructMainEx2.treatment,
  generators      = c(1, 1),
  effect          = "main")

ConstructMainEx2
summary(ConstructMainEx2)}

# Construct Main + Some Ex. 1
\dontrun{
ConstructMainSomeEx1.treatment <- matrix(c(
  0, 0, 0, 0,
  2, 2, 0, 2,
  3, 0, 0, 0,
  1, 1, 0, 1,
  1, 0, 2, 0,
  3, 2, 1, 0,
  2, 1, 0, 0,
  0, 0, 0, 2,
  3, 0, 0, 1,
  0, 2, 2, 1,
  1, 2, 0, 0,
  1, 0, 1, 2,
  2, 0, 2, 0,
  2, 0, 1, 1,
  3, 1, 2, 2,
  0, 1, 1, 0),
  nrow = 16, byrow = TRUE)

ConstructMainSomeEx1 <- sb.design(
  operation       = "construct", 
  nattributes     = 4,
  nlevels         = c(4, 3, 3, 3),
  nalternatives   = 2,
  attribute.names = list(A = c("a0", "a1", "a2", "a3"), 
                         B = c("b0", "b1", "b2"),
                         C = c("c0", "c1", "c2"),
                         D = c("d0", "d1", "d2")),
  design          = ConstructMainSomeEx1.treatment,
  generators      = c(3, 2, 2, 2),
  effect          = "mplussome",
  interactions    = c(1, 2),
  determinant     = "1")

ConstructMainSomeEx1
summary(ConstructMainSomeEx1)}

# Construct Main + Some Ex. 2
\dontrun{
ConstructMainSomeEx2.treatment <- matrix(c(
  0, 0,
  0, 1,
  1, 0,
  1, 1), 
  nrow = 4, byrow = TRUE)

ConstructMainSomeEx2 <- sb.design(
  operation       = "construct", 
  nattributes     = 2,
  nlevels         = c(2, 2),
  nalternatives   = 2,
  attribute.names = list(X = c("x0", "x1"), 
                         Y = c("y0", "y1")),
  design          = ConstructMainSomeEx2.treatment,
  generators      = matrix(c(0, 1, 1, 1), nrow = 2, byrow = TRUE),
  effect          = "mplussome",
  interactions    = c(1, 2),
  determinant     = NULL)

ConstructMainSomeEx2
summary(ConstructMainSomeEx2)}
}
\keyword{ design }
