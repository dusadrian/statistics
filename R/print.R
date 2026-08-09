print.ttesthv <- function(x, ...) {
    homogtest <- x$homogtest
    ttest <- x$ttest
    ttestWelch <- x$ttestWelch
    paired <- x$paired
    var.equal <- x$var.equal
    conf.level <- x$conf.level


    if (is.null(var.equal) && !paired && !is.null(homogtest)) {
        p.value <- homogtest$p.value
        var.equal <- p.value > (1 - conf.level)

        cat(paste0(
            "\nThe homogeneity of variances test has a p-value of ",
            round(p.value, 4),
            ".\n"
        ))
    }

    if (is.null(var.equal)) {
        var.equal <- TRUE
    }

    if (isTRUE(var.equal)) {
        print(ttest)
    } else {
        print(ttestWelch)
    }

    cat("\n")
}


`print.anovahv` <- function(x, ...) {
    p.value <- x$homog_test$p.value
    var.equal <- x$var.equal
    conf.level <- x$conf.level

    if (is.null(var.equal)) {
        var.equal <- p.value > 1 - conf.level
    }

    if (is.null(x$var.equal)) {
        cat(paste0(
            "\nThe homogeneity of variances test has a p-value of ",
            round(p.value, 4),
            ".\n\n"
        ))
    }

    if (isTRUE(var.equal)) {
        cat("ANOVA table\n\n")
        print(summary(x$test))
        cat("\n\n")
    }
    else {
        cat("ANOVA table using Welch approximation\n\n")
        print(noquote(x$output_table))
        cat("\n")
    }

    if (!is.null(x$numsum)) {
        cat("Numerical summaries\n\n")
        print(x$numsum, startend = FALSE)
        cat("\n\n")
    }
}
