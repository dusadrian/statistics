\name{dbinoms}
\alias{dbinoms}

\title{Calculate probabilities and draw graphics for a binomial distribution}

\description{
This function draws graphics for a certain number of repetitions of an experiment,
at a certain probability of success, and calculates the probability of obtaining
one or more values from a random variable.
}

\usage{
dbinoms(x, size, prob, log = FALSE, draw = FALSE,
        zoom = FALSE, new = FALSE, text = FALSE)
}

\arguments{
  \item{x}{One or more integer outcomes. Repeated values count once.}
  \item{size}{Nonnegative integer number of trials, including a single Bernoulli trial.}
  \item{prob}{Probability of success}
  \item{log}{Logical; if TRUE, the probability is returned as log(p)}
  \item{draw}{Logical; if TRUE, draws the binomial distribution}
  \item{zoom}{Logical; if TRUE, eliminates from the graphic all numbers
  with probability equal to zero}
  \item{new}{Logical; if TRUE, a new window will be created for each graphic}
  \item{text}{Logical; if TRUE, display the probability above each bar}
}

\value{The numeric probability of the requested set of outcomes. Positive
probabilities below 0.0001 print as \code{"< 0.0001"} and retain their full
calculated precision. With \code{log = TRUE}, the result is the numeric
natural logarithm. The empty set has probability zero. Bars always show probabilities.}

\author{Adrian Dusa}

\seealso{\code{\link{dbinom}}}

\examples{
# All 20 tosses of a fair coin come up heads
p <- dbinoms(20, 20, 0.5)
p                             # prints < 0.0001
dbinoms(20, 20, 0.5, log = TRUE)

# 8 repetitions, with a 0.5 probability of success, calculate the
# probability of obtaining between 2 and 4 favourable outcomes
dbinoms(2:4, 8, 0.5)

# less than 7 favourable outcomes
dbinoms(0:6, 8, 0.5)

#at most 7 favourable outcomes
dbinoms(0:7, 8, 0.5)

# above 5 favourable outcomes
dbinoms(6:8, 8, 0.5)

# at least 5 favourable outcomes
dbinoms(5:8, 8, 0.5)

# exactly 6 favourable outcomes
dbinoms(6, 8, 0.5)

# 1, 3 or 6 favourable outcomes
dbinoms(c(1, 3, 6), 8, 0.5)

# same, drawing the graphic
dbinoms(c(1, 3, 6), 8, 0.5, draw = TRUE)

# same, drawing the probabilities in the graphic
dbinoms(c(1, 3, 6), 8, 0.5, draw = TRUE, text = TRUE)

}

\keyword{misc}

