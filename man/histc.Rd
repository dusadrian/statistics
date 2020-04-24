\name{histc}

\alias{histc}

\title{Histogram with a superimposed normal curve}

\description{
Draws a histogram with a normal curve that approximates the
distribution.
}

\usage{
histc(x, from, to, size = 15, ...)
}

\arguments{
    \item{x}{Numeric vector}
    \item{from}{Starting point on the horizontal axis.}
    \item{to}{End point on the horizontal axis.}
    \item{size}{Size of the graphic, in centimeters.}
    \item{...}{Other parameters, specific to the base \bold{\code{hist()}} function.}
}


\author{
Adrian Dusa
}

\examples{
x <- sample(18:93, 150, replace = TRUE)

histc(x)

histc(x, 10, 100)

histc(x, 10, 100, xlab = "Age", ylab = "Frequency",
      main = "Histogram for age in years")
}

\keyword{functions}
