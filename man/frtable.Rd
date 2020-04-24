\name{frtable}
\alias{frtable}

\title{Frequency table}

\description{
Presents the usual frequency distributions from a frequency table.
}

\usage{
frtable(x)
}

\arguments{
  \item{x}{A numerical vector}
}

\author{Adrian Dusa}


\examples{
x <- sample(1:5, 103, replace = TRUE)

frtable(x)

y <- factor(sample(letters[1:5], 115, replace = TRUE))
frtable(y)
}

\keyword{functions}

