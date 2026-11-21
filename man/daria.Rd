\name{daria}
\alias{daria}

\title{Calculate and draw the area under the normal curve z}

\description{
  The function "daria" - 'd'raws the 'area' under the normal curve for certain values of z.
}
\usage{
daria(area, z1, z2, draw = FALSE, format = NULL)
}

\arguments{
  \item{area}{The required area}
  \item{z1}{First z value}
  \item{z2}{Second z value}
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

The drawing extends from -4 to 4. The probability uses the supplied cutoffs.
}

\value{
A numeric probability with its full calculated precision. Small positive results
have a print method for classroom display. With \code{format = TRUE},
the return value is a character label.
}

\author{Adrian Dusa}

\seealso{\code{\link{pnorm}}, \code{\link{qnorm}}}

\examples{
# A probability small enough to describe as practically zero
p <- daria("over", 9)
p              # prints < 0.0001
log(p)         # uses the full numeric probability

daria("between", -1.96, 1.96)

daria("over", -1)

daria("under", -1)

daria("over", 2, draw = TRUE)

}

\keyword{misc}
