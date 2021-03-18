\name{using}

\alias{using}

\title{Evaluate an expression in a data environment}

\description{
A function almost identical to the base function \code{with()}, but allowing
to evaluate the expression in every subset of a split file.
}

\usage{
using(data, expr, split.by = NULL, ...)
}

\arguments{
    \item{data}{A data frame.}
    \item{expr}{Expression to evaluate}
    \item{split.by}{A factor variable from the \code{data}}
    \item{...}{Other internal arguments.}
}

\value{
Returns a list of results.
}


\author{
Adrian Dusa
}

\examples{
set.seed(123)
df <- data.frame(
    Area = factor(sample(c("Urban", "Rural"), 20, replace = TRUE)),
    Gender = factor(sample(c("Male", "Female"), 20, replace = TRUE)),
    Age = sample(18:90, 20, replace = TRUE),
    Children = sample(0:5, 20, replace = TRUE)
)

# table of frequencies for Area
fretab(df$Gender)

# same with
using(df, fretab(Gender))

# same, but split by Area
using(df, fretab(Gender), split.by = Area)

# calculate the mean age by gender
using(df, mean(Age), split.by = Gender)

# same, but select cases from the urban area
using(subset(df, Area == "Urban"), mean(Age), split.by = Gender)

# mean age by gender and area
using(df, mean(Age), split.by = Area & Gender)

# same with
using(df, mean(Age), split.by = c(Area, Gender))

# average number of children by Area
using(df, mean(Children), split.by = Area)

# frequency tables by Area
using(df, fretab(Children), split.by = Area)
}

\keyword{functions}
