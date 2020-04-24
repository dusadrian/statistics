\name{frelevel}
\alias{frelevel}

\title{Modified \code{relevel()} function}
\description{
  The base function \code{relevel()} accepts a single argument "ref" (in other
  words, that argument can only be a scalar not a vector of values).
  frelevel() accepts more levels (even all) and reorders them function of their
  order in the vector.
}
\usage{
frelevel(variable, levels)
}

\arguments{
  \item{variable}{The categorical variable of interest}
  \item{levels}{One or more levels of the factor, in the desired order}
}

\value{A factor of the same length as the initial one.}

\author{Adrian Dusa}

\seealso{relevel}

\examples{
words <- c("ini", "mini", "miny", "moe")
variable <- factor(words, levels = words)

# modify the order of the levels, keeping the order of the values
frelevel(variable, c("moe", "ini", "miny", "mini"))

}

\keyword{misc}

