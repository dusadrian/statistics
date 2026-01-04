\name{t.testAB}
\alias{t.testAB}
\alias{t_testAB}

\title{Student's t test with a preliminary test for the homogeneity of variances}

\description{
  This function executes the t test for one or two groups. In case of two
  independent groups, the function verifies if the group variances are equal,
  using the Ansari-Bradley test.
}

\usage{
\method{t}{testAB}(
  x, y = NULL,
  alternative = c("two.sided", "less", "greater"),
  data = NULL, ...
)
}

\arguments{
  \item{x}{A numeric vector.}
  \item{y}{An optional numeric vector, corresponding to the second group.}
  \item{alternative}{Character, for the alternative hypothesis.
    See details below.}
  \item{data}{An optional matrix or a set of data containing the variables from
    a formula}
  \item{...}{Other arguments for compatibility with the base R function \code{t.test},
    see Details below.}
}

\details{
The argument \code{alternative} follows the standard in the base function
\bold{\code{t.test}()}, and it can be \code{"two.sided"}, \code{"less"} or
\code{"greater"}. In addition to those options, this function also allows for
\code{"!="} and \code{"two.tailed"} for the bidirectional alternative hypothesis,
as well as \code{"<"} and \code{"lower"} for the one tailed test on the left tail,
and \code{">"} and \code{"higher"} for the right tailed test, respectively.

Additional arguments can be provided via the three dots \code{...} argument, all of these being passed to the base R function \code{t.test()}.

For instance, one such argument is the logical \code{var.equal} (either TRUE or FALSE), function of which R will opt for a Welch test (if unequal variations, the default), or the classical Student t test when the populations are assumed to have equal variances. In the absence of this argument, a test for the homogeneity of variances is performed.

Another argument of interest could be the logical \code{paired}, indicating if the two samples are independent (default) or paired. In case of paired samples, it is not possible to specify the input using a formula.
}


\author{Adrian Dusa}

\examples{

group1 <- c(13, 14,  9, 12,  8, 10,  5, 10,  9, 12, 16)
group2 <- c(16, 18, 11, 19, 14, 17, 13, 16, 17, 18, 22, 12)

t.testAB(group1, group2)


# or, if the variables are inside a dataset
dataset <- data.frame(
  values = c(group1, group2),
  group = c(rep(1,11), rep(2,12))
)

# using a formula
t.testAB(values ~ group, data = dataset)

using(
  dataset,
  t.testAB(values ~ group)
)

}

\keyword{misc}
