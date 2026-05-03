\name{t.testhv}
\alias{t.testAB}
\alias{t.testhv}
\alias{t.testhv.default}
\alias{t.testhv.formula}
\alias{t.testhv.Pair}

\title{Student's t test with a homogeneity of variances check}

\description{
  Performs one-sample, two-sample and paired t tests, with additional support for
  choosing between the classical Student t test and the Welch t test. For two
  independent samples, the default behavior is to first run the Ansari-Bradley
  test for the homogeneity of variances.
}

\usage{
\method{t}{testhv}(x, ...)

\method{t.testhv}{default}(
  x, y = NULL,
  alternative = c("two.sided", "less", "greater"),
  mu = 0, paired = FALSE, var.equal = NULL, conf.level = 0.95,
  ...
)

\method{t.testhv}{formula}(
  formula, data, subset, na.action = na.pass, ...
)

\method{t.testhv}{Pair}(
  x,
  alternative = c("two.sided", "less", "greater"),
  mu = 0, var.equal = NULL, conf.level = 0.95,
  ...
)
}

\arguments{
  \item{x}{A numeric vector, a \code{Pair} object for paired tests, or the first
  argument dispatched by the generic method.}

  \item{y}{An optional second numeric vector. If \code{y} is a categorical
  object of class \code{"declared"}, it is treated as a grouping variable and
  \code{x} is split into two samples.}

  \item{formula}{A formula. Use \code{values ~ group} for two independent
  samples, \code{values ~ 1} for a one-sample test, or
  \code{Pair(before, after) ~ 1} for a paired test.}

  \item{data}{An optional data frame, matrix or list containing the variables
  in \code{formula}.}

  \item{subset}{An optional vector specifying a subset of observations.}

  \item{na.action}{A function specifying how missing values should be handled
  by the formula method.}

  \item{alternative}{Character string specifying the alternative hypothesis.
  See Details.}

  \item{mu}{A number indicating the true value of the mean, difference in
  means, or mean difference under the null hypothesis.}

  \item{paired}{Logical, indicating whether the two supplied numeric vectors are
  paired. This argument is not accepted by the formula method; use
  \code{Pair(before, after) ~ 1} instead.}

  \item{var.equal}{Logical or \code{NULL}. If \code{TRUE}, the classical
  Student t test with pooled variance is printed. If \code{FALSE}, the Welch
  test is printed. If \code{NULL}, the default, an Ansari-Bradley test is used
  to decide which of the two tests to print.}

  \item{conf.level}{Numeric value between 0 and 1, giving the confidence level
  of the interval.}

  \item{...}{Additional arguments passed to the relevant method.}
}

\details{
The main difference from base R's \code{\link[stats]{t.test}} is the default
treatment of \code{var.equal}. In \code{t.test()}, this argument is logical and
defaults to \code{FALSE}, meaning Welch's unequal-variance test is used unless
the user explicitly requests the pooled-variance Student test.

In \code{t.testhv()}, \code{var.equal} defaults to \code{NULL}. This leaves the
choice to the function: for two independent samples it first applies
\code{\link[stats]{ansari.test}} as a homogeneity of variances test, then prints
the pooled-variance Student test when the variances are considered homogeneous,
or the Welch test otherwise. Users can still set \code{var.equal = TRUE} or
\code{var.equal = FALSE} to mimic the explicit behavior of
\code{\link[stats]{t.test}}.

Internally, both the pooled-variance and Welch tests are computed and stored in
the result object. The print method decides which one to display, based on
\code{var.equal} and, when \code{var.equal = NULL}, the p-value of the
homogeneity test.

The \code{alternative} argument accepts the same standard values as
\code{\link[stats]{t.test}}: \code{"two.sided"}, \code{"less"} and
\code{"greater"}. In addition, \code{"!="} and \code{"two.tailed"} are accepted
for a two-sided test, \code{"<"} and \code{"lower"} for a left-tailed test, and
\code{">"}, \code{"higher"} and \code{"upper"} for a right-tailed test.

Formula calls support categorical grouping variables of class \code{"declared"}.
Such variables are coerced to factors before splitting the response into two
groups. The default method also supports the shortcut
\code{t.testhv(values, group)} when \code{group} is a \code{"declared"}
categorical variable with exactly two levels.

Paired t tests can be requested either with two numeric vectors and
\code{paired = TRUE}, or by supplying a \code{Pair} object. In the formula
method, paired tests should be written as \code{Pair(before, after) ~ 1};
\code{paired = TRUE} is deliberately rejected in formula calls.
}

\value{
An object of class \code{"ttesthv"}, containing:
\itemize{
  \item \code{homogtest}: the Ansari-Bradley homogeneity of variances test, or
  \code{NULL} when it is not relevant;
  \item \code{ttest}: the pooled-variance Student t test;
  \item \code{ttestWelch}: the Welch t test;
  \item \code{paired}, \code{var.equal} and \code{conf.level}: the corresponding
  analysis settings.
}
}

\author{Adrian Dusa}

\seealso{
\code{\link[stats]{t.test}}, \code{\link[stats]{ansari.test}},
\code{\link[stats]{Pair}}
}

\examples{
group1 <- c(13, 14,  9, 12,  8, 10,  5, 10,  9, 12, 16)
group2 <- c(16, 18, 11, 19, 14, 17, 13, 16, 17, 18, 22, 12)

t.testhv(group1, group2)

# Force the same variance choice as base R's t.test()
t.testhv(group1, group2, var.equal = FALSE)
t.testhv(group1, group2, var.equal = TRUE)


# Formula interface
dataset <- data.frame(
  values = c(group1, group2),
  group = c(rep(1, 11), rep(2, 12))
)

t.testhv(values ~ group, data = dataset)

using(
  dataset,
  t.testhv(values ~ group)
)


# Declared categorical grouping variables are detected
dataset$declared_group <- declared(
  dataset$group,
  labels = c(Group1 = 1, Group2 = 2)
)

t.testhv(values ~ declared_group, data = dataset)
t.testhv(dataset$values, dataset$declared_group)


# Paired tests
before <- c(8, 7, 6, 9, 10)
after <- c(7, 6, 7, 6, 8)

t.testhv(before, after, paired = TRUE)
t.testhv(Pair(before, after))

paired_data <- data.frame(before = before, after = after)
t.testhv(Pair(before, after) ~ 1, data = paired_data)
}

\keyword{misc}
