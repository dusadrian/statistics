`t.testAB` <- function(
    x, y = NULL,
    alternative = c("two.sided", "less", "greater"),
    data = NULL, ...
) {

    dots <- list(...)
    paired <- isTRUE(dots$paired)

    if (identical(alternative, c("two.sided", "less", "greater"))) {
        alternative <- "two.sided"
    }

    if (is.element(alternative, c("two.sided", "!=", "two.tailed"))) {
        pmatching <- 1
    }
    else if (is.element(alternative, c("less", "<", "lower"))) {
        pmatching <- 2
    }
    else if (is.element(alternative, c("greater", ">", "higher", "upper"))) {
        pmatching <- 3
    }
    else {
        admisc::stopError("Unknown alternative hypothesis specification.")
    }

    conf.level <- dots$conf.level
    if (is.null(conf.level)) {
        conf.level <- 0.95
    } else {
        if (!admisc::possibleNumeric(conf.level)) {
            admisc::stopError("The confidence level should be numeric.")
        }

        conf.level <- admisc::asNumeric(conf.level)

        if (conf.level < 0 | conf.level > 1) {
            admisc::stopError("The confidence level should be in the interval [0 - 1].")
        }
    }

    alternative <- c("two.sided", "less", "greater")[pmatching]

    var.equal <- dots$var.equal

    homogtest <- NULL

    if (is.null(var.equal) && !paired) {
        homogeneity <- c("not equal", "equal")

        if (is.null(y)) {
            homogtest <- ansari.test(x, data = data, exact = FALSE)
        } else {
            homogtest <- ansari.test(x, y, exact = FALSE)
        }
    }

    callist <- list(x, y)

    if (is.null(y)) {
        callist <- list(formula = x)
        if (paired) {
            admisc::stopError("Cannot use 'paired' in formula method.")
        }
    } else {
        callist$paired <- paired
    }

    callist$alternative <- dots$alternative
    callist$conf.level <- dots$conf.level
    callist$var.equal <- TRUE
    callist$data <- data
    ttest <- do.call("t.test", callist)

    callist$var.equal <- FALSE
    ttestWelch <- do.call("t.test", callist)


    result <- list(
        homogtest = homogtest,
        ttest = ttest,
        ttestWelch = ttestWelch,
        paired = paired,
        var.equal = var.equal,
        conf.level = conf.level
    )

    class(result) <- "ttestAB"
    return(result)
}

`t_testAB` <- function(...) {
    t.testAB(...)
}