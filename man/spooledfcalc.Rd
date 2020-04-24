\name{dfcalc and spooled}
\alias{dfcalc}
\alias{spooled}

\title{Calculates the degrees of freedom and the pooled variation for a t test}

\description{
The function \code{dfcalc} is used only for two samples t test, when the group
variations are NOT equal.
For small and independent samples, and unknown but equal population variances, the
variances of the two samples are used. As the sample variances are never equal, this
function calculates their pooled variance based on the two standard deviations and
their respective sample sizes.
}

\usage{
dfcalc(x, y, n1, n2)
spooled(x, y, n1, n2)
}

\arguments{
  \item{x}{The values of the standard deviation for the first group}
  \item{y}{The values of the standard deviation for the second group}
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

