\name{frmode}

\alias{frmode}
\alias{fmode}

\title{Calculates the (frequency) mode of a variable}

\description{
The mode of a variable can be calculated for any level of measurement,
but it is based on the values' "fr"equencies.
}

\usage{
frmode(x)
}

\arguments{
    \item{x}{A factor, or a numeric / character vector.}
}

\value{
Returns the first value having the highest frequency.
}


\author{
Adrian Dusa
}

\examples{
age <- sample(18:93, 150, replace = TRUE)
frmode(age)

# gender as a categorical variable
gender <- factor(sample(c("Male", "Female"), 100, replace = TRUE))

# the modal category
frmode(gender)

# the modal value
frmode(as.numeric(gender))
}

\keyword{functions}
