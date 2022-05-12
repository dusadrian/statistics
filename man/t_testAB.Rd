\name{t_testAB}
\alias{t_testAB}

\title{Student's t test with a preliminary testing for the homogeneity of variances}

\description{
  This function executes the t test for one or two groups. In case of two
  independent groups, the function verifies if the group variances are equal,
  using the Ansari-Bradley test.
}

\usage{
t_testAB(x, y=NULL,
         alternative = c("two.sided", "less", "greater"), var.equal = FALSE,
         mu = 0, paired = FALSE, conf.level = 0.95, data)
}

\arguments{
  \item{x}{A numeric vector.}
  \item{y}{An optional numeric vector, corresponding to the second group.}
  \item{alternative}{Character, for the alternative hypothesis. It can be "two.sided"
    or "!=" for a two tailed test, "less" or "<" for a one tailed test on the left,
    and "greater" or ">" for a one tailed test on the right.}
  \item{var.equal}{Logical argument indicating whether to treat the two variances as being equal}
  \item{mu}{A number indicating the true value of the mean (or difference in means if performing a two sample test).}
  \item{paired}{Logical indicating whether to perform a paired t-test.}
  \item{conf.level}{Confidence level of the interval}
  \item{data}{An optional matrix or a set of data containing the variables from a formula}
}


\author{Adrian Dusa}

\examples{

group1 <- c(13, 14,  9, 12,  8, 10,  5, 10,  9, 12, 16)
group2 <- c(16, 18, 11, 19, 14, 17, 13, 16, 17, 18, 22, 12)

t_testAB(group1, group2)


# or, if the variables are inside a dataset
dataset <- data.frame(
  values = c(group1, group2),
  group = c(rep(1,11), rep(2,12))
)

t_testAB(values ~ group, data = dataset)

}

\keyword{misc}
