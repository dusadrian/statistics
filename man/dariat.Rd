\name{dariat}
\alias{dariat}

\title{Calculate and draw the area under the t distribution}
\description{
  A function similar to "daria", with the only difference it uses the t instead of the
  z distribution. In addition, the function expects an additional parameter for the
  degrees of freedom. 
}
\usage{
dariat(area, t1, t2, df, draw = FALSE)
}

\arguments{
  \item{area}{The required area}
  \item{t1}{First t value, in the interval +/- 4}
  \item{t2}{Second t value, in the interval +/- 4}
  \item{df}{Degrees of freedom}
  \item{draw}{Logical; if TRUE, draw the area}
}

\details{
In the argument \bold{\code{area}}, the function accepts:

\code{"l"}, \code{"u"}, \code{"left"} and \code{"under"} for the area to the left of z,

\code{"r"}, \code{"o"}, \code{"a"}, \code{"right"} \code{"over"} and \code{"above"} for the area to the right of z

\code{"b"} and \code{"between"} for the area between two z values.

z values smaller than -4 and greater than +4 are truncated to these values, since the area
to the left and to the right of these values is practically equal to zero.
}

\author{Adrian Dusa}

\seealso{\code{\link{pt}}, \code{\link{qt}}}

\examples{
# for 100 degrees of freedom
dariat("between", -1.96, 1.96, df = 100)

dariat("over", -1, df = 100)

dariat("under", -1, df = 100)

dariat("over", 2, df = 100, draw = TRUE)

}

\keyword{misc}

