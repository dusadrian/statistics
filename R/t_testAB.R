`t_testAB` <- function(
    x, y = NULL,
    alternative = c("two.sided", "less", "greater"),
    data = NULL, ...
) {

    dots <- list(...)

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

    if (is.null(var.equal) && !isTRUE(dots$paired)) {
        homogeneity <- c("not equal", "equal")

        if (is.null(y)) {
            homogtest <- ansari.test(x, data = data, exact = FALSE)
        } else {
            homogtest <- ansari.test(x, y, exact = FALSE)
        }

        p.value <- homogtest$p.value
        var.equal <- p.value > (1 - conf.level)
        cat (
            "\nThe homogeneity of variances test has a p-value of ",
            round(p.value, 4),
            ", variances are ",
            homogeneity[var.equal + 1],
            ".\n",
            sep = ""
        )
    }

    callist <- list(x, y)

    if (is.null(y)) {
        callist <- list(formula = x)
        if (isTRUE(dots$paired)) {
            admisc::stopError("Cannot use 'paired' in formula method.")
        }
    } else {
        callist$paired <- isTRUE(dots$paired)
    }

    callist$alternative <- dots$alternative
    callist$conf.level <- dots$conf.level
    callist$var.equal <- isTRUE(var.equal)
    callist$data <- data
    test <- do.call("t.test", callist)

    print(test)

    if (isTRUE(callist$paired)) {
        return(invisible(list("t.test" = test)))
    }
    else if (is.null(dots$var.equal)) {
        return(
            invisible(
                list("Homogeneity of variances" = homogtest, "t.test" = test)
            )
        )
    }
}
