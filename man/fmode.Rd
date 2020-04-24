\name{fmode}

\alias{fmode}
\alias{mod}

\title{Calculates the mode of a variable}

\description{
The mode of a variable can be calculated for any level of measurement,
but it is based on the values "f"requencies.
}

\usage{
fmode(x)
}

\arguments{
    \item{x}{A vector of any type.}
}

\value{
Returns the first value having the highest frequency.
}


\author{
Adrian Dusa
}

\examples{
age <- sample(18:93, 150, replace = TRUE)
fmode(age)

# gender as a categorical variable
gender <- factor(sample(c("Male", "Female"), 100, replace = TRUE))

# the modal category
fmode(gender)

# the modal value
fmode(as.numeric(gender))
}

\keyword{functions}
