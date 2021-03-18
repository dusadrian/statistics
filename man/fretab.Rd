\name{fretab}
\alias{fret}
\alias{fretab}
\alias{frtable}

\title{Frequency table}

\description{
Presents the usual frequency distributions from a frequency table.
}

\usage{
fretab(x)
}

\arguments{
  \item{x}{A categorical variable, a factor}
}

\details{
A frequency table is usually performed for a categorical variable, displaying the
frequencies of the respective categories. Note that general variables containing
text are not necessarily factors, despite having a small number of words.

It is sometimes possible to perform a frequency table for numerical variables, if
and only if the number of values is very limited (an arbitrary and debatable upper
limit of 15 is used). An example of such variable can be the number of children,
where each value can be interpreted as a class, containing a single value (for
instance 0 meaning the category of people with no children).

A special type of input variables are those of class \code{haven_labelled}, which
are not pure factors (categorical variables in R) but they are nevertheless
intepreted as similar to factors, and allow producing frequency tables for them.
Importing data from SPSS using the package \pkg{haven} usually produces such
variables.
}

\author{Adrian Dusa}


\examples{
# a pure categorical variable
x <- factor(sample(letters[1:5], 115, replace = TRUE))
fretab(x)

# simulate number of children
y <- sample(0:4, 103, replace = TRUE)
fretab(y)
}

\keyword{functions}
