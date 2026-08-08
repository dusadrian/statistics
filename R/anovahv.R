`anovahv` <- function(x, ...) {
    UseMethod("anovahv")
}


`anovahv.default` <- function(
    x, y, var.equal = NULL, conf.level = 0.95, ...
) {

    if (missing(y)) {
        admisc::stopError("The grouping variable is missing.")
    }

    result <- .anovahv_fit(
        values = x,
        group = y,
        var.equal = var.equal,
        conf.level = conf.level,
        dname = paste(deparse1(substitute(x)), "by", deparse1(substitute(y)))
    )

    return(result)
}


`anovahv.formula` <- function(
    formula, data, subset, na.action = na.omit,
    var.equal = NULL, conf.level = 0.95,
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
        dname = dname,
        values_name = mf_names[response],
        group_name = mf_names[-response]
    )

    return(result)
}


`.anovahv_fit` <- function(
    values, group, var.equal = NULL, conf.level = 0.95, dname = NULL,
    values_name = "values_x", group_name = "group_y"
) {

    if (!is.null(var.equal) && !is.logical(var.equal)) {
        admisc::stopError("The 'var.equal' argument should be logical or NULL.")
    }

    conf.level <- .anovahv_conf_level(conf.level)

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

    result <- list(
        homog_test = homog_test,
        test = test,
        welch = welch,
        output_table = output_table,
        var.equal = var.equal,
        conf.level = conf.level,
        data.name = dname
    )

    class(result) <- "anovahv"

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
