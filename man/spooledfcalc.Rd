\name{dfcalc and spooled}
\alias{dfcalc}
\alias{spooled}

\title{Welch degrees of freedom and pooled standard deviation}

\description{
Calculate Welch-Satterthwaite degrees of freedom with \code{dfcalc}, or the
pooled sample standard deviation assuming equal variances with \code{spooled}.
Supply two raw samples or two standard deviations and their sample sizes.
Missing observations in a raw sample propagate to the result. Welch degrees
of freedom are undefined when both sample variances are zero.
}

\usage{
dfcalc(x, y, n1, n2)
spooled(x, y, n1, n2)
}

\arguments{
  \item{x}{Raw sample or nonnegative standard deviation for the first group}
  \item{y}{Raw sample or nonnegative standard deviation for the second group}
  \item{n1}{Size of the first group}
  \item{n2}{Size of the second group}
}

\author{Adrian Dusa}

\examples{
group1 <- c(13, 14,  9, 12,  8, 10,  5, 10,  9, 12, 16)
group2 <- c(16, 18, 11, 19, 14, 17, 13, 16, 17, 18, 22, 12)
sd1 <- sd(group1)
sd2 <- sd(group2)
n1 <- length(group1)
n2 <- length(group2)

# more direct
dfcalc(group1, group2)

# if the standard deviations and group sizes are known
dfcalc(sd1, sd2, n1, n2)

# the pooled standard deviation
spooled(sd1, sd2, n1, n2)

# more direct
spooled(group1, group2)
}

\keyword{misc}
