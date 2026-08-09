`anovahv` <- function(x, ...) {
    UseMethod("anovahv")
}


`anovahv.default` <- function(
    x, y, var.equal = NULL, conf.level = 0.95, numsum = FALSE, ...
) {

    if (missing(y)) {
        admisc::stopError("The grouping variable is missing.")
    }

    result <- .anovahv_fit(
        values = x,
        group = y,
        var.equal = var.equal,
        conf.level = conf.level,
        numsum = numsum,
        dname = paste(deparse1(substitute(x)), "by", deparse1(substitute(y)))
    )

    return(result)
}


`anovahv.formula` <- function(
    formula, data, subset, na.action = na.omit,
    var.equal = NULL, conf.level = 0.95, numsum = FALSE,
    ...
) {

    if (missing(formula) || length(formula) != 3L) {
        admisc::stopError("The formula is incorrect.")
    }

    if (length(attr(terms(formula[-2L]), "term.labels")) != 1L) {
        admisc::stopError("The formula is incorrect.")
    }

    m <- match.call(expand.dots = FALSE)

    if (is.matrix(eval(m$data, parent.frame()))) {
        m$data <- as.data.frame(data)
    }

    m[[1L]] <- quote(stats::model.frame)
    m$... <- NULL
    m$var.equal <- NULL
    m$conf.level <- NULL
    m$numsum <- NULL

    summary_mf <- NULL
    if (isTRUE(numsum)) {
        summary_call <- m
        summary_call$na.action <- quote(stats::na.pass)
        summary_mf <- eval(summary_call, parent.frame())
        names(summary_mf) <- NULL
    }

    mf <- eval(m, parent.frame())
    mf_names <- vapply(
        names(mf), admisc::getName, character(1)
    )
    dname <- paste(mf_names, collapse = " by ")
    names(mf) <- NULL

    response <- attr(attr(mf, "terms"), "response")

    result <- .anovahv_fit(
        values = mf[[response]],
        group = mf[[-response]],
        var.equal = var.equal,
        conf.level = conf.level,
        numsum = numsum,
        dname = dname,
        values_name = mf_names[response],
        group_name = mf_names[-response],
        summary_values = if (is.null(summary_mf)) NULL else summary_mf[[response]],
        summary_group = if (is.null(summary_mf)) NULL else summary_mf[[-response]]
    )

    return(result)
}


`.anovahv_fit` <- function(
    values, group, var.equal = NULL, conf.level = 0.95, numsum = FALSE,
    dname = NULL, values_name = "values_x", group_name = "group_y",
    summary_values = NULL, summary_group = NULL
) {

    if (!is.null(var.equal) && !is.logical(var.equal)) {
        admisc::stopError("The 'var.equal' argument should be logical or NULL.")
    }

    if (!is.logical(numsum) || length(numsum) != 1L || is.na(numsum)) {
        admisc::stopError("The 'numsum' argument should be TRUE or FALSE.")
    }

    conf.level <- .anovahv_conf_level(conf.level)

    numerical_summaries <- NULL
    if (numsum) {
        if (is.null(summary_values)) {
            summary_values <- values
        }
        if (is.null(summary_group)) {
            summary_group <- group
        }
        numerical_summaries <- .anovahv_numsum(
            summary_values,
            summary_group
        )
    }

    # unnecessary for declared >= 0.27 but protects against using
    # earlier versions of declared package
    if (is.declared(values)) {
        values <- drop_na(values)
    }

    if (is.declared(group)) {
        group <- as.factor(group)
    }
    else if (!is.factor(group)) {
        group <- as.factor(group)
    }

    testdf <- na.omit(
        data.frame(values_x = values, group_y = group)
    )
    names(testdf) <- c(values_name, group_name)
    test_formula <- reformulate(group_name, response = values_name)

    if (nlevels(testdf[[group_name]]) < 2L) {
        admisc::stopError("The grouping variable should have at least two levels.")
    }

    homog_test <- fligner.test(test_formula, data = testdf)
    test <- aov(test_formula, data = testdf)
    tabel <- anova(lm(test_formula, data = testdf))
    welch <- oneway.test(test_formula, data = testdf)

    output_table <- as.data.frame(matrix(NA, nrow = 2, ncol = 5))
    rownames(output_table) <- c(group_name, "Residuals")
    colnames(output_table) <- c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")

    output_table[ , 1] <- welch$parameter
    output_table[ , 2] <- round(tabel[["Sum Sq"]], 2)
    output_table[ , 3] <- round(tabel[["Mean Sq"]], 2)
    output_table[1, 4] <- round(welch$statistic, 4)
    output_table[1, 5] <- round(welch$p.value, 4)

    output_table[1, 3] <- output_table[1, 4] * output_table[2, 3]
    output_table[1, 2] <- output_table[1, 3] * output_table[1, 1]
    output_table[2, 4] <- ""
    output_table[2, 5] <- ""

    # Keep the fitted ANOVA model as the result's underlying object. This lets
    # methods for "aov" and "lm" work without an anovahv-specific adapter.
    result <- test
    result$homog_test <- homog_test
    result$test <- test
    result$welch <- welch
    result$output_table <- output_table
    result$numsum <- numerical_summaries
    result$var.equal <- var.equal
    result$conf.level <- conf.level
    result$data.name <- dname

    class(result) <- c("anovahv", class(test))

    return(result)
}


`.anovahv_numsum` <- function(values, group) {

    if (is.declared(group)) {
        group <- as.factor(group)
    }
    else if (!is.factor(group)) {
        group <- as.factor(group)
    }

    result <- do.call(
        rbind,
        lapply(
            split(values, group),
            wmeasures,
            what = c("mean", "sd", "n", "NA")
        )
    )
    class(result) <- c("wmeasures", class(result))

    return(result)
}


`.anovahv_conf_level` <- function(conf.level = 0.95) {

    if (is.null(conf.level)) {
        conf.level <- 0.95
    }

    if (!admisc::possibleNumeric(conf.level)) {
        admisc::stopError("The confidence level should be numeric.")
    }

    conf.level <- admisc::asNumeric(conf.level)

    if (conf.level < 0 | conf.level > 1) {
        admisc::stopError("The confidence level should be in the interval [0 - 1].")
    }

    return(conf.level)
}

`anovaFK` <- function(...)  {
    anovahv(...)
}
