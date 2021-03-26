\name{fretab}
\alias{fretab}
\alias{frtable}

\title{Frequency table}

\description{
Presents the usual frequency distributions from a frequency table.
}

\usage{
fretab(x, values = FALSE)
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
# a pure categorical variable
x <- factor(sample(letters[1:5], 215, replace = TRUE))
fretab(x)


# simulate number of children
x <- sample(0:4, 215, replace = TRUE)
fretab(x)

library(haven)
values <- sample(c(1:5, 9), 215, replace = TRUE)
x <- labelled(values, labels = c(Good = 1, Bad = 5, DK = 9))
fretab(x)


# Defining missing values (without labels)
x <- labelled_spss(values, labels = c(Good = 1, Bad = 5), na_values = 9)
fretab(x)


# Defined missing values with labels
x <- labelled_spss(values, labels = c(Good = 1, Bad = 5, DK = 9), na_values = 9)
fretab(x)


# Printing both values and labels
fretab(x, values = TRUE)


# Using tagged_na values (similar to Stata or SAS)
values <- sample(c(1:5, tagged_na('a')), 215, replace = TRUE)

x <- labelled(values, labels = c(Good = 1, Bad = 5, DK = tagged_na('a')))
fretab(x, values = TRUE)


# And even a combination between tagged_na() and labelled_spss()
tags <- tagged_na('a', 'b')
values <- sample(c(1:5, -9, tags), 215, replace = TRUE)
labels <- c(Good = 1, Bad = 5, Refuse = -9, DK = tags[1], NR = tags[2])

x <- labelled_spss(values, labels = labels, na_values = c(-9, tags))
fretab(x, values = TRUE)

}

\keyword{functions}
