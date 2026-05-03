`t.testhv` <- function(x, ...) {
    UseMethod("t.testhv")
}


`t.testhv.default` <- function(
    x, y = NULL,
    alternative = c("two.sided", "less", "greater"),
    mu = 0, paired = FALSE, var.equal = NULL, conf.level = 0.95,
    ...
) {

    args <- .ttesthv_args(
        alternative = alternative,
        conf.level = conf.level
    )

    alternative <- args$alternative
    conf.level <- args$conf.level
    dname <- deparse1(substitute(x))

    if (!is.null(y) && !paired && .is_declared_categorical(y)) {
        dname <- paste(
            dname,
            "by",
            deparse1(substitute(y))
        )
        group <- as.factor(y)
        x <- .ttesthv_values(x)

        if (nlevels(group) != 2L) {
            admisc::stopError("The grouping variable must have exactly 2 levels.")
        }

        DATA <- split(x, group)

        result <- t.testhv(
            DATA[[1L]],
            DATA[[2L]],
            alternative = alternative,
            mu = mu,
            var.equal = var.equal,
            conf.level = conf.level,
            ...
        )

        if (length(result$ttest$estimate) == 2L) {
            names(result$ttest$estimate) <- paste("mean in group", levels(group))
            names(result$ttest$null.value) <- paste(
                "difference in means between",
                paste("group", levels(group), collapse = " and ")
            )
        }

        if (length(result$ttestWelch$estimate) == 2L) {
            names(result$ttestWelch$estimate) <- paste("mean in group", levels(group))
            names(result$ttestWelch$null.value) <- paste(
                "difference in means between",
                paste("group", levels(group), collapse = " and ")
            )
        }

        result$ttest$data.name <- dname
        result$ttestWelch$data.name <- result$ttest$data.name

        return(result)
    }

    x <- .ttesthv_values(x)

    if (!is.null(y)) {
        dname <- paste(
            dname,
            "and",
            deparse1(substitute(y))
        )
        y <- .ttesthv_values(y)
    }

    homogtest <- NULL

    if (is.null(var.equal) && !paired && !is.null(y)) {
        homogtest <- ansari.test(x, y, exact = FALSE)
    }

    callist <- list(
        x = x,
        y = y,
        alternative = alternative,
        mu = mu,
        paired = paired,
        conf.level = conf.level,
        var.equal = TRUE
    )

    if (is.null(y)) {
        callist$y <- NULL
        callist$paired <- NULL
    }

    ttest <- do.call("t.test", callist)

    callist$var.equal <- FALSE
    ttestWelch <- do.call("t.test", callist)

    ttest$data.name <- dname
    ttestWelch$data.name <- dname

    result <- list(
        homogtest = homogtest,
        ttest = ttest,
        ttestWelch = ttestWelch,
        paired = paired,
        var.equal = var.equal,
        conf.level = conf.level
    )

    class(result) <- "ttesthv"

    return(result)
}


`t.testhv.formula` <- function(
    formula, data, subset, na.action = na.pass, ...
) {

    if (missing(formula) || length(formula) != 3L) {
        admisc::stopError("The formula is incorrect.")
    }

    if ("paired" %in% ...names()) {
        admisc::stopError("Cannot use 'paired' in formula method.")
    }

    dots <- list(...)

    args <- .ttesthv_args(
        alternative = getElement(dots, "alternative"),
        conf.level = getElement(dots, "conf.level")
    )

    mu <- getElement(dots, "mu")
    if (is.null(mu)) {
        mu <- 0
    }

    var.equal <- getElement(dots, "var.equal")

    oneSampleOrPaired <- FALSE

    if (length(attr(terms(formula[-2L]), "term.labels")) != 1L) {
        if (formula[[3L]] == 1L) {
            oneSampleOrPaired <- TRUE
        }
        else {
            admisc::stopError("The formula is incorrect.")
        }
    }

    m <- match.call(expand.dots = FALSE)

    if (is.matrix(eval(m$data, parent.frame()))) {
        m$data <- as.data.frame(data)
    }

    m[[1L]] <- quote(stats::model.frame)
    m$... <- NULL

    mf <- eval(m, parent.frame())
    dname <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL

    response <- attr(attr(mf, "terms"), "response")
    respVar <- mf[[response]]

    if (oneSampleOrPaired) {
        result <- t.testhv(
            respVar,
            alternative = args$alternative,
            mu = mu,
            var.equal = var.equal,
            conf.level = args$conf.level
        )
    }
    else {
        group <- mf[[-response]]
        respVar <- .ttesthv_values(respVar)

        if (is.declared(group)) {
            group <- as.factor(group)
        }
        else if (!is.factor(group)) {
            group <- factor(group)
        }

        if (nlevels(group) != 2L) {
            admisc::stopError("The grouping variable must have exactly 2 levels.")
        }

        DATA <- split(respVar, group)

        result <- t.testhv(
            DATA[[1L]],
            DATA[[2L]],
            alternative = args$alternative,
            mu = mu,
            var.equal = var.equal,
            conf.level = args$conf.level
        )

        if (length(result$ttest$estimate) == 2L) {
            names(result$ttest$estimate) <- paste("mean in group", levels(group))
            names(result$ttest$null.value) <- paste(
                "difference in means between",
                paste("group", levels(group), collapse = " and ")
            )
        }

        if (length(result$ttestWelch$estimate) == 2L) {
            names(result$ttestWelch$estimate) <- paste("mean in group", levels(group))
            names(result$ttestWelch$null.value) <- paste(
                "difference in means between",
                paste("group", levels(group), collapse = " and ")
            )
        }
    }

    result$ttest$data.name <- dname
    result$ttestWelch$data.name <- dname

    return(result)
}


`t.testhv.Pair` <- function(
    x,
    alternative = c("two.sided", "less", "greater"),
    mu = 0, var.equal = NULL, conf.level = 0.95,
    ...
) {

    result <- t.testhv(
        x = x[, 1L],
        y = x[, 2L],
        alternative = alternative,
        mu = mu,
        paired = TRUE,
        var.equal = var.equal,
        conf.level = conf.level,
        ...
    )

    return(result)
}


`.ttesthv_args` <- function(
    alternative = c("two.sided", "less", "greater"),
    conf.level = 0.95
) {

    if (is.null(alternative)) {
        alternative <- c("two.sided", "less", "greater")
    }

    if (is.null(conf.level)) {
        conf.level <- 0.95
    }

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

    if (!admisc::possibleNumeric(conf.level)) {
        admisc::stopError("The confidence level should be numeric.")
    }

    conf.level <- admisc::asNumeric(conf.level)

    if (conf.level < 0 | conf.level > 1) {
        admisc::stopError("The confidence level should be in the interval [0 - 1].")
    }

    return(list(
        alternative = c("two.sided", "less", "greater")[pmatching],
        conf.level = conf.level
    ))
}


`.is_declared_categorical` <- function(x) {

    if (!is.declared(x)) {
        return(FALSE)
    }

    measurement <- measurement(x)

    return(
        any(
            grepl(
                "categorical|nominal|ordinal",
                measurement
            )
        )
    )
}


`.ttesthv_values` <- function(x) {

    if (is.declared(x)) {
        attributes(x) <- NULL
    }

    return(x)
}

`t.testAB` <- function(...)  {
    t.testhv(...)
}