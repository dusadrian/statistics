\name{frtable}
\alias{frtable}
\alias{fretable}

\title{Create a frequency table}

\description{
Presents the usual frequency distributions from a frequency table.
}

\usage{
frtable(x, values = TRUE)
}

\arguments{
  \item{x}{A categorical variable, either a factor or a \code{declared} variable}
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

A special type of input variables are those of class \code{declared}, which
are not pure factors (categorical variables in R) but they are nevertheless
intepreted as similar to factors, and allow producing frequency tables for.

The argument \code{values} makes sense only when the input is of class
\code{declared}, otherwise for regular (base R) factors the values are
just a sequence of numbers.
}

\author{Adrian Dusa}


\examples{

# a pure categorical variable
x <- factor(sample(letters[1:5], 215, replace = TRUE))
frtable(x)


# simulate number of children
x <- sample(0:4, 215, replace = TRUE)
frtable(x)

values <- sample(c(1:7, 99), 215, replace = TRUE)
x <- declared(values, labels = c("Good" = 1, "Bad" = 7))
frtable(x)


# Defining missing values
missing_values(x) <- 99
frtable(x)


# Defined missing values with labels
values <- sample(c(1:7, 99), 215, replace = TRUE)
x <- declared(values, labels = c("Good" = 1, "Bad" = 7, "Don't know" = 99))
missing_values(x) <- 99
frtable(x)


# Printing without values
frtable(x, values = FALSE)


}

\keyword{functions}
