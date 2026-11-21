.small_probability <- function(p) {
    length(p) == 1L && isTRUE(p > 0 && p < 0.0001)
}

.probability_result <- function(p, format = NULL) {
    if (isTRUE(format)) return(.format_probability(p))
    if (is.null(format) && .small_probability(p)) {
        return(structure(p, class = c("statistics_probability", "numeric")))
    }
    p
}

print.statistics_probability <- function(x, ...) {
    value <- unclass(x)
    if (.small_probability(value)) cat(.format_probability(value), "\n", sep = "")
    else print(value, ...)
    invisible(x)
}

# The classroom label belongs to the returned probability. Arithmetic and mathematical
# functions should return their usual numeric or logical results.
Ops.statistics_probability <- function(e1, e2) {
    if (inherits(e1, "statistics_probability")) e1 <- unclass(e1)
    if (!missing(e2) && inherits(e2, "statistics_probability")) e2 <- unclass(e2)
    NextMethod()
}

Math.statistics_probability <- function(x, ...) {
    x <- unclass(x)
    NextMethod()
}

.format_probability <- function(p, p_value = FALSE) {
    if (!is.finite(p)) return("unavailable")
    if (.small_probability(p) || (p_value && p == 0)) return("< 0.0001")
    format(round(p, 4L), scientific = FALSE, trim = TRUE)
}

print.anovahv <- function(x, ...) {
    pooled <- if (!is.null(x$selected)) inherits(x$selected, "aov") else
        .select_pooled(x$var.equal, x$homog_test$p.value, x$conf.level, is.null(x$welch))
    if (is.null(x$var.equal)) {
        cat("\nFligner-Killeen test for equal variances, p-value ",
            .format_probability(x$homog_test$p.value, p_value = TRUE), "\n\n", sep = "")
    }
    if (pooled) {
        cat("ANOVA table\n\n")
        old <- options(scipen = 99)
        on.exit(options(old), add = TRUE)
        print(summary(x$test)[[1L]], eps.Pvalue = 0.0001, ...)
        if (!is.null(x$welch_unavailable)) cat(x$welch_unavailable, "\n")
    } else {
        cat("ANOVA table using Welch approximation\n\n")
        tab <- .welch_table(x$welch)
        tab[["Pr(>F)"]] <- .format_probability(x$welch$p.value, p_value = TRUE)
        print(tab, row.names = FALSE, ...)
    }
    if (!is.null(x$numsum)) {
        cat("\nNumerical summaries\n")
        print(x$numsum, startend = FALSE)
    }
    cat("\n")
    invisible(x)
}

print.ttesthv <- function(x, ...) {
    if (!is.null(x$homogtest)) {
        cat("\nAnsari-Bradley scale test (assumes a common location), p-value ",
            .format_probability(x$homogtest$p.value, p_value = TRUE), "\n", sep = "")
    }
    selected <- x$selected
    if (is.null(selected)) {
        pooled <- .select_pooled(x$var.equal, x$homogtest$p.value,
            x$conf.level, is.null(x$homogtest))
        selected <- if (pooled) x$ttest else x$ttestWelch
    }
    if (isTRUE(selected$p.value < 0.0001)) {
        selected$p.value <- NULL
        print(selected, ...)
        cat("p-value < 0.0001\n")
    } else {
        print(selected, ...)
    }
    if (!is.null(x$welch_unavailable)) cat(x$welch_unavailable, "\n")
    invisible(x)
}
