\name{anovaFK}
\alias{anovaFK}

\title{ANOVA including homogeneity of variance test}
\description{
  The function 'anovaFK' - contains two separate tests: in a first state, the
  Fligner-Killeen test for the homogeneity of variances is run, and function of this
  test, the Welch approximation is applied if the groups are not homogeneous.
}
\usage{
anovaFK(x, y = NULL, data)
}

\arguments{
  \item{x}{A vector of values or a formula object as in 'lhs ~ rhs', unde 'lhs'
  contains the values and the 'rhs' contains the groups. Both can be vectors or variables
  from a dataset.}
  \item{y}{An optional vector of values, when the two variables are not specified using
  a formula object.}
  \item{data}{A dataset containing the variables specified in the formula object, in case
  they don't exist as separate objects.}
}

\details{
When the variances are not equal, the output differs from the one presented by
oneway.test, but the table is similar.

If the degrees of freedom are not what they should be (\eqn{k - 1}, and \eqn{n - k}
respectively) something must be wrong. Specifically, the grouping variable should be
declared as a factor (in case it is not already character), otherwise it is considered
metric and a regression model is applied instead of ANOVA.
                                                            
Declaring a variable as a factor is done using the command: \code{as.factor}
}

\author{Adrian Dusa}

\seealso{
\code{\link{aov}}, \code{\link{anova}}, \code{\link{oneway.test}}, \code{\link{fligner.test}}
}

\examples{

values <- c(15, 8, 17, 7, 26, 12, 8, 11, 16, 9, 16, 24, 20, 19, 9, 17, 11, 8, 15, 6, 14)
groups <- rep(1:3, each = 7)
groups <- as.factor(groups)

anovaFK(values ~ groups)

# acelasi lucru cu:
anovaFK(values, groups)

# daca variabilele se gasesc intr-un set de date
my.data <- data.frame(values, groups)
anovaFK(values ~ groups, data = my.data)

}

\keyword{misc}

