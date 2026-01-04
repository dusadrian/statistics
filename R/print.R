print.ttestAB <- function(x, ...) {
    homogtest <- x$homogtest
    ttest <- x$ttest
    paired <- x$paired
    var.equal <- x$var.equal
    conf.level <- x$conf.level


    if (is.null(var.equal) && !paired) {
        p.value <- homogtest$p.value
        homogeneity <- c("not equal", "equal")
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

    print(ttest)
    cat("\n")
}


`print.anovaFK` <- function(x, ...) {
    homog_test <- x$homog_test
    homogeneity <- c("do not have equal variation.", "have equal variation.")
    dots <- list(...)

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

    var_equal <- homog_test$p.value > 1 - conf.level

    cat(
        "\nThe Fligner-Killeen test for the homogeneity of variances has a ",
        paste(
            "p-value = ",
            round(homog_test$p.value, 4),
            sep = ""
        ),
        ", groups have ",
        ifelse(var_equal, "", "un"),
        "equal variations.",
        "\n\n",
        sep = ""
    )

    if (var_equal) {
        cat("ANOVA table\n\n")
        print(summary(x$test))
        cat("\n\n")
    }
    else {
        cat("ANOVA table using Welch approximation\n\n")
        print(noquote(x$output_table))
        cat("\n")
    }
}
