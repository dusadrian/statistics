\name{anovahv}
\alias{anovaFK}
\alias{anovahv}
\alias{anovahv.default}
\alias{anovahv.formula}

\title{ANOVA with a homogeneity of variances check}

\description{
  Performs one-way ANOVA together with a Fligner-Killeen test for the
  homogeneity of variances. When the group variances are not considered
  homogeneous, the print method displays a Welch approximation table.
}

\usage{
anovahv(x, ...)

\method{anovahv}{default}(
  x, y, var.equal = NULL, conf.level = 0.95, ...
)

\method{anovahv}{formula}(
  formula, data, subset, na.action = na.omit,
  var.equal = NULL, conf.level = 0.95,
  ...
)
}

\arguments{
  \item{x}{A numeric response vector.}

  \item{y}{A grouping variable. Character variables and categorical variables
  of class \code{"declared"} are coerced to factors. Other non-factor grouping
  variables are also treated as factors.}

  \item{formula}{A formula of the form \code{values ~ group}.}

  \item{data}{An optional data frame, matrix or list containing the variables
  in \code{formula}.}

  \item{subset}{An optional vector specifying a subset of observations.}

  \item{na.action}{A function specifying how missing values should be handled
  by the formula method.}

  \item{var.equal}{Logical or \code{NULL}. If \code{TRUE}, the classical ANOVA
  table is printed. If \code{FALSE}, the Welch approximation table is printed.
  If \code{NULL}, the default, the Fligner-Killeen test is used to decide which
  table to print.}

  \item{conf.level}{Numeric value between 0 and 1. When
  \code{var.equal = NULL}, the homogeneity test is evaluated against
  \code{1 - conf.level}.}

  \item{...}{Additional arguments passed to methods.}
}

\details{
The function is a teaching-oriented wrapper around \code{\link[stats]{aov}},
\code{\link[stats]{lm}}, \code{\link[stats]{oneway.test}} and
\code{\link[stats]{fligner.test}}. It always stores both the classical ANOVA
result and the Welch approximation result. The print method chooses which one
to display.

The argument \code{var.equal} follows the same general idea as in
\code{\link{t.testhv}}. If it is left as \code{NULL}, the function performs a
Fligner-Killeen homogeneity of variances test and prints the classical ANOVA
table when the variances are considered homogeneous. If \code{var.equal = TRUE}
or \code{var.equal = FALSE}, the user explicitly chooses the classical ANOVA or
Welch output, respectively.

Grouping variables of class \code{"declared"} are detected and coerced with
\code{as.factor()} before fitting the models. This allows declared categorical
variables to be used directly as group variables in both the default and formula
interfaces.
}

\value{
An object inheriting from classes \code{"anovahv"}, \code{"aov"} and
\code{"lm"}. The underlying object is the classical ANOVA model, allowing
standard model generics such as \code{summary()}, \code{TukeyHSD()},
\code{predict()}, \code{residuals()} and \code{fitted()} to dispatch to their
existing \code{"aov"} or \code{"lm"} methods. For compatibility, it also
contains:
\itemize{
  \item \code{homog_test}: the Fligner-Killeen homogeneity of variances test;
  \item \code{test}: the classical ANOVA model produced by \code{aov};
  \item \code{welch}: the Welch one-way test produced by \code{oneway.test};
  \item \code{output_table}: a compact Welch approximation table;
  \item \code{var.equal} and \code{conf.level}: the analysis settings.
}
}

\author{Adrian Dusa}

\seealso{
\code{\link[stats]{aov}}, \code{\link[stats]{anova}},
\code{\link[stats]{oneway.test}}, \code{\link[stats]{fligner.test}},
\code{\link{t.testhv}}
}

\examples{
values <- c(
  15, 8, 17, 7, 26, 12, 8, 11, 16, 9, 16,
  24, 20, 19, 9, 17, 11, 8, 15, 6, 14
)
groups <- rep(1:3, each = 7)

anovahv(values, groups)
anovahv(values ~ groups)

# Force a specific output, bypassing the variance decision
anovahv(values, groups, var.equal = TRUE)
anovahv(values, groups, var.equal = FALSE)


# Using a dataset
vgdf <- data.frame(values, groups)

anovahv(values ~ groups, data = vgdf)

using(
  vgdf,
  anovahv(values ~ groups)
)


# Declared categorical grouping variables are detected
vgdf$declared_groups <- declared(
  vgdf$groups,
  labels = c(A = 1, B = 2, C = 3)
)

anovahv(values ~ declared_groups, data = vgdf)
anovahv(vgdf$values, vgdf$declared_groups)


# Class example
cls <- data.frame(
  values = c(
    22, 27, 32, 30, 29, 27, 33, 24, 24, 30,
    28, 22, 24, 18, 21, 26, 25, 20, 24, 28,
    20, 28, 31, 26, 26, 30, 21, 25, 29, 27
  ),
  groups = rep(1:3, each = 10)
)

using(
  cls,
  anovahv(values ~ groups)
)

result <- anovahv(values ~ groups, data = cls)
TukeyHSD(result)

# Post-hoc test, for example Bonferroni
using(
  cls,
  pairwise.t.test(values, groups, p.adj = "bonferroni")
)
}

\keyword{misc}
