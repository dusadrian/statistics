\name{frtable}
\alias{frtable}
\alias{fretable}

\title{Frequency table}

\description{
Presents the usual frequency distributions from a frequency table.
}

\usage{
frtable(x, values = TRUE)
}

\arguments{
  \item{x}{A categorical variable, either a factor or a \code{labelled} variable}
  \item{values}{Logical, print the values in the table rows}
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
intepreted as similar to factors, and allow producing frequency tables for.
Importing data from SPSS using the package \pkg{haven} usually produces such
variables.

The argument \code{values} makes sense only when the input is of class
\code{haven_labelled}, otherwise for regular (base R) factors the values are
just a sequence of numbers.
}

\author{Adrian Dusa}


\examples{
library(labelled)

# a pure categorical variable
x <- factor(sample(letters[1:5], 215, replace = TRUE))
frtable(x)


# simulate number of children
x <- sample(0:4, 215, replace = TRUE)
frtable(x)

values <- sample(c(1:5, 9), 215, replace = TRUE)
x <- labelled(values, labels = c(Good = 1, Bad = 5))
frtable(x)


# Defining missing values
na_values(x) <- 9
frtable(x)


# Defined missing values with labels
x <- labelled(values, labels = c(Good = 1, Bad = 5, DK = 9))
na_values(x) <- 9
frtable(x)


# Printing without values
frtable(x, values = FALSE)


# Using tagged_na values (similar to Stata or SAS)
values <- sample(c(1:5, tagged_na('a')), 215, replace = TRUE)

x <- labelled(values, labels = c("Good" = 1, "Bad" = 5, "Don't know" = tagged_na('a')))
frtable(x)

}

\keyword{functions}
