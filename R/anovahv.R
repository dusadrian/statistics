anovahv <- function(x, ...) {
    UseMethod("anovahv")
}

anovahv.default <- function(
    x, y, var.equal = NULL, conf.level = 0.95, numsum = FALSE, ...
) {
    .check_dots(...)
    if (missing(y) || length(x) != length(y)) {
        stop("Supply one group label for each observation.", call. = FALSE)
    }
    frame <- stats::model.frame(values_x ~ group_y,
        data = list(values_x = x, group_y = y), na.action = stats::na.pass)
    dname <- paste(.expr_name(substitute(x)), "by", .expr_name(substitute(y)))
    .anovahv_fit(frame, na.omit, var.equal, conf.level, numsum, match.call(), dname)
}

anovahv.formula <- function(
    formula, data, subset, na.action = na.omit,
    var.equal = NULL, conf.level = 0.95, numsum = FALSE, ...
) {
    .check_dots(...)
    original <- match.call()
    m <- original[c(1L, match(c("formula", "data", "subset"), names(original), 0L))]
    m[[1L]] <- quote(stats::model.frame)
    if (!missing(data)) {
        m$data <- if (is.matrix(data)) as.data.frame(data) else data
    }
    m$na.action <- quote(stats::na.pass)
    frame <- eval(m, parent.frame())
    terms <- attr(frame, "terms")
    if (length(frame) != 2L || attr(terms, "response") != 1L ||
        attr(terms, "intercept") != 1L || length(attr(terms, "offset"))) {
        stop("Use a one-way formula of the form 'values ~ group'.", call. = FALSE)
    }
    .anovahv_fit(frame, na.action, var.equal, conf.level, numsum, original)
}

.anovahv_fit <- function(frame, na.action, var.equal, conf.level, numsum, original,
    dname = paste(names(frame), collapse = " by ")) {
    .check_flag(var.equal, "var.equal", allow_null = TRUE)
    .check_flag(numsum, "numsum")
    conf.level <- .conf_level(conf.level)
    if (!is.numeric(frame[[1L]]) || !is.null(dim(frame[[1L]]))) {
        stop("The response should be a numeric vector.", call. = FALSE)
    }

    # model.frame has already evaluated the data, transformations and subset.
    # Keep those terms for prediction, applying the same group conversion there.
    terms <- attr(frame, "terms")
    if (!is.factor(frame[[2L]])) {
        frame[[2L]] <- as.factor(frame[[2L]])
        predvars <- attr(terms, "predvars")
        predvars[[3L]] <- call("as.factor", predvars[[3L]])
        attr(terms, "predvars") <- predvars
        attr(terms, "dataClasses")[2L] <- "factor"
    }
    numerical_summaries <- NULL
    if (numsum) {
        numerical_summaries <- do.call(rbind, lapply(split(frame[[1L]], frame[[2L]]),
            wmeasures, what = c("mean", "sd", "n", "NA")))
        class(numerical_summaries) <- c("wmeasures", class(numerical_summaries))
    }

    if (!is.null(na.action)) {
        frame <- match.fun(na.action)(frame)
    }
    # Even na.pass needs complete pairs for the variance and means tests.
    if (anyNA(frame[[1L]]) || anyNA(frame[[2L]])) frame <- na.omit(frame)
    group <- frame[[2L]]
    sizes <- tabulate(group, nbins = nlevels(group))
    if (any(sizes == 0L)) {
        group <- frame[[2L]] <- droplevels(group)
        sizes <- sizes[sizes > 0L]
    }
    values <- frame[[1L]]
    if (nlevels(group) < 2L) {
        stop("The grouping variable should have at least two observed levels.", call. = FALSE)
    }
    # Ordinary omission historically returns the compact fit. na.exclude also
    # retains the original row positions for fitted values and residuals.
    if (inherits(attr(frame, "na.action"), "omit")) attr(frame, "na.action") <- NULL
    attr(frame, "terms") <- terms
    test <- aov(frame)
    original[[1L]] <- quote(statistics::anovahv)
    test$call <- original
    homog_test <- fligner.test(values, group)
    welch <- NULL
    if (all(sizes >= 2L)) {
        candidate <- oneway.test(values ~ group)
        if (is.finite(candidate$p.value)) welch <- candidate
    }
    reason <- "Welch ANOVA requires at least two observations and positive variance in every group."
    if (isFALSE(var.equal) && is.null(welch)) stop(reason, call. = FALSE)
    pooled <- .select_pooled(var.equal, homog_test$p.value, conf.level, is.null(welch))

    result <- test
    result$data.name <- dname
    homog_test$data.name <- result$data.name
    if (!is.null(welch)) {
        welch$data.name <- result$data.name
        result$output_table <- .welch_table(welch)
    }
    result$test <- test
    result["welch"] <- list(welch)
    result$homog_test <- homog_test
    result$selected <- if (pooled) test else welch
    if (is.null(welch)) result$welch_unavailable <- reason
    result$numsum <- numerical_summaries
    result$var.equal <- var.equal
    result$conf.level <- conf.level
    class(result) <- c("anovahv", class(test))
    result
}

.welch_table <- function(welch) {
    data.frame(
        "F value" = unname(welch$statistic),
        "num df" = unname(welch$parameter[1L]),
        "denom df" = unname(welch$parameter[2L]),
        "Pr(>F)" = welch$p.value, check.names = FALSE
    )
}

model.tables.anovahv <- function(x, ...) {
    # model.tables.aov indexes the frame with the term labels, which include
    # backticks for non-syntactic names. Keep the public model frame intact.
    fit <- x$test
    names(fit$model) <- rownames(attr(fit$terms, "factors"))
    stats::model.tables(fit, ...)
}

anovaFK <- function(...) {
    anovahv(...)
}
