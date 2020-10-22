\name{daria}
\alias{daria}

\title{Calculate and draw the area under the normal curve z}
\description{
  The function "daria" - 'd'raws the 'area' under the normal curve for certain values of z.
}
\usage{
daria(area, z1, z2, draw = FALSE)
}

\arguments{
  \item{area}{The required area}
  \item{z1}{First z value, in the interval +/- 4}
  \item{z2}{Second z value, in the interval +/- 4}
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

\seealso{\code{\link{pnorm}}, \code{\link{qnorm}}}

\examples{
daria("between", -1.96, 1.96) 

daria("over", -1)

daria("under", -1)

daria("over", 2, draw = TRUE)

}

\keyword{misc}

