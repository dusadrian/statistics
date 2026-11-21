\name{dariat}
\alias{dariat}

\title{Calculate and draw the area under the t distribution}
\description{
  A function similar to \code{daria}, with the only difference it uses the t instead of the
  normal z distribution. In addition, the function expects an additional parameter for the
  degrees of freedom.
}
\usage{
dariat(area, t1, t2, df, draw = FALSE, format = NULL)
}

\arguments{
  \item{area}{The required area}
  \item{t1}{First t value}
  \item{t2}{Second t value}
  \item{df}{Degrees of freedom}
  \item{draw}{Logical. Draw the requested area when true.}
  \item{format}{With the default \code{NULL}, a positive probability below 0.0001
  prints as \code{"< 0.0001"}. Use \code{FALSE} for a plain numeric value or
  \code{TRUE} for a character label.}
}

\details{
In the argument \bold{\code{area}}, the function accepts:

\code{"l"}, \code{"u"}, \code{"left"} and \code{"under"} for the area to the left of z,

\code{"r"}, \code{"o"}, \code{"a"}, \code{"right"} \code{"over"} and \code{"above"} for the area to the right of z

\code{"b"} and \code{"between"} for the area between two z values.

The drawing extends from -5 to 5 for fewer than 21 degrees of freedom and from -4 to 4 otherwise. The probability uses the supplied cutoffs.
}

\value{
A numeric probability with its full calculated precision. Small positive results
have a print method for classroom display. With \code{format = TRUE},
the return value is a character label.
}

\author{Adrian Dusa}

\seealso{\code{\link{pt}}, \code{\link{qt}}}

\examples{
# A probability small enough to describe as practically zero
p <- dariat("over", 20, df = 100)
p              # prints < 0.0001
log(p)         # uses the full numeric probability

# for 100 degrees of freedom
dariat("between", -1.96, 1.96, df = 100)

dariat("over", -1, df = 100)

dariat("under", -1, df = 100)

dariat("over", 2, df = 100, draw = TRUE)

}

\keyword{misc}

