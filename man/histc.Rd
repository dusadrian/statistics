\name{histc}

\alias{histc}

\title{Histogram with a superimposed normal curve}

\description{
Draws a histogram with a normal curve that approximates the
distribution.
When \code{ylim} is not provided, it is chosen to accommodate both the
histogram bars and the superimposed curve.
}

\usage{
histc(x, from, to, size = 15, ...)
}

\arguments{
    \item{x}{Numeric vector}
    \item{from}{Starting point on the horizontal axis.}
    \item{to}{End point on the horizontal axis.}
    \item{size}{Side length in centimeters when opening a new device.}
    \item{...}{Histogram arguments such as \code{breaks}, \code{freq}, \code{probability},
    \code{axes}, \code{labels} and \code{plot}. With \code{plot = FALSE} the
    histogram is calculated without opening a device.}
}


\details{
Each axis limit can be supplied separately. All finite observations contribute
to the histogram and normal fit. On a density axis the curve is the fitted
normal density. On a count axis the density is multiplied by the sample size
and the bin width. The curve is omitted when the sample SD is zero or undefined.
}
\value{The histogram object, returned invisibly when drawn.}

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
