\name{mbinom and sbinom}
\alias{mbinom}
\alias{sbinom}

\title{Calculates the mean and the standard deviation of a discreet random variable}

\description{
  The function expects a tabel (a data frame or a matrix) with just two columns:
  the first containing the values of a random variable, and the associated
  probabilities in the second column.
}

\usage{
mbinom(x)
sbinom(x)
}

\arguments{
  \item{x}{The data table.}
}

\details{
  If the sum of the probabilities on the second columns is not equal to 1, the function
  interprets them as absolute values and recalculates the relative frequencies.
}

\author{Adrian Dusa}

\examples{

data <- matrix(c(0:4, 0.015, 0.235, 0.425, 0.245, 0.080), ncol = 2)
mbinom(data)
sbinom(data)

data <- data.frame(X = 0:4, P_X = c(12, 188, 340, 196, 64))
mbinom(data)
sbinom(data) 

}

\keyword{misc}

