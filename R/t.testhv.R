t.testhv <- function(x, ...) {
    UseMethod("t.testhv")
}

t.testhv.default <- function(
    x, y = NULL, alternative = c("two.sided", "less", "greater"),
    mu = 0, paired = FALSE, var.equal = NULL, conf.level = 0.95, ...
) {
    .check_dots(...)
    .check_flag(paired, "paired")
    .check_flag(var.equal, "var.equal", allow_null = TRUE)
    conf.level <- .conf_level(conf.level)
    choices <- c("two.sided" = "two.sided", "!=" = "two.sided", "two.tailed" = "two.sided",
        "less" = "less", "<" = "less", "lower" = "less",
        "greater" = "greater", ">" = "greater", "higher" = "greater", "upper" = "greater")
    if (is.null(alternative) || identical(alternative, c("two.sided", "less", "greater"))) {
        alternative <- "two.sided"
    }
    if (length(alternative) != 1L || !alternative %in% names(choices)) {
        stop("Unknown alternative hypothesis specification.", call. = FALSE)
    }
    alternative <- unname(choices[alternative])
    dname <- .expr_name(substitute(x))
    if (!paired && !is.null(y) && (is.factor(y) || is.character(y) ||
        (is.declared(y) && any(grepl("categorical|nominal|ordinal", measurement(y)))))) {
        return(.ttest_groups(x, y, paste(dname, "by", .expr_name(substitute(y))),
            alternative = alternative, mu = mu, var.equal = var.equal, conf.level = conf.level))
    }
    if (!is.numeric(x) || !is.null(dim(x)) ||
        (!is.null(y) && (!is.numeric(y) || !is.null(dim(y))))) {
        stop("Supply numeric sample vectors.", call. = FALSE)
    }
    if (!is.null(y)) dname <- paste(dname, "and", .expr_name(substitute(y)))
    if (paired && is.null(y)) stop("Supply both samples for a paired t test.", call. = FALSE)
    if (is.declared(x)) x <- as.numeric(x)
    if (is.declared(y)) y <- as.numeric(y)

    independent <- !paired && !is.null(y)
    homog <- NULL
    if (independent && is.null(var.equal)) homog <- ansari.test(x, y, exact = FALSE)
    pooled <- tryCatch(t.test(x, y, alternative = alternative, mu = mu, paired = paired,
        var.equal = TRUE, conf.level = conf.level), error = identity)
    if (inherits(pooled, "error") && (!independent || isTRUE(var.equal))) stop(pooled)
    welch <- pooled
    if (independent) {
        welch <- tryCatch(t.test(x, y, alternative = alternative, mu = mu,
            conf.level = conf.level), error = identity)
    }
    reason <- NULL
    if (inherits(welch, "error")) {
        if (isFALSE(var.equal) || inherits(pooled, "error")) stop(welch)
        reason <- paste("Welch's t test is unavailable:", conditionMessage(welch))
        welch <- NULL
    }
    if (inherits(pooled, "error")) pooled <- NULL
    if (!is.null(pooled)) pooled$data.name <- dname
    if (!is.null(welch)) welch$data.name <- dname
    use_pooled <- !independent || (!is.null(pooled) &&
        .select_pooled(var.equal, homog$p.value, conf.level, is.null(welch)))
    structure(list(homogtest = homog, ttest = pooled, ttestWelch = welch,
        selected = if (use_pooled) pooled else welch, welch_unavailable = reason,
        paired = paired, var.equal = var.equal, conf.level = conf.level), class = "ttesthv")
}

.ttest_groups <- function(x, group, dname, ...) {
    if (!is.numeric(x) || !is.null(dim(x))) stop("Supply a numeric response vector.", call. = FALSE)
    if (length(x) != length(group)) {
        stop("Supply one group label for each observation.", call. = FALSE)
    }
    group <- droplevels(as.factor(group))
    if (nlevels(group) != 2L) stop("The grouping variable must have exactly two observed levels.",
        call. = FALSE)
    # The design is already determined. Use numeric responses in both samples.
    if (is.declared(x)) x <- as.numeric(x)
    samples <- split(x, group)
    result <- t.testhv(samples[[1L]], samples[[2L]], ...)
    for (name in c("ttest", "ttestWelch", "selected")) {
        if (is.null(result[[name]])) next
        names(result[[name]]$estimate) <- paste("mean in group", levels(group))
        names(result[[name]]$null.value) <- paste("difference in means between",
            paste("group", levels(group), collapse = " and "))
        result[[name]]$data.name <- dname
    }
    result
}

t.testhv.formula <- function(formula, data, subset, na.action = na.pass, ...,
    alternative = "two.sided", mu = 0, var.equal = NULL, conf.level = 0.95) {
    if ("paired" %in% names(substitute(list(...)))) {
        stop("Use Pair(before, after) ~ 1 for a paired formula test.", call. = FALSE)
    }
    .check_dots(...)
    if (is.null(mu)) mu <- 0
    original <- match.call()
    m <- original[c(1L, match(c("formula", "data", "subset"), names(original), 0L))]
    m[[1L]] <- quote(stats::model.frame)
    if (!missing(data)) m$data <- if (is.matrix(data)) as.data.frame(data) else data
    m$na.action <- na.action
    frame <- eval(m, parent.frame())
    if (length(formula) == 3L && isTRUE(all.equal(formula[[3L]], 1))) {
        result <- t.testhv(frame[[1L]], alternative = alternative, mu = mu,
            var.equal = var.equal, conf.level = conf.level)
        for (name in c("ttest", "ttestWelch", "selected")) {
            result[[name]]$data.name <- names(frame)[1L]
        }
        return(result)
    }
    if (length(formula) != 3L || length(frame) != 2L) {
        stop("Use 'values ~ group', 'values ~ 1' or 'Pair(before, after) ~ 1'.",
            call. = FALSE)
    }
    .ttest_groups(frame[[1L]], frame[[2L]], paste(names(frame), collapse = " by "),
        alternative = alternative, mu = mu, var.equal = var.equal, conf.level = conf.level)
}

t.testhv.Pair <- function(
    x, alternative = c("two.sided", "less", "greater"),
    mu = 0, var.equal = NULL, conf.level = 0.95, ...
) {
    if (missing(alternative)) alternative <- "two.sided"
    t.testhv(x[, 1L], x[, 2L], alternative = alternative, mu = mu,
        paired = TRUE, var.equal = var.equal, conf.level = conf.level, ...)
}

t.testAB <- function(...) {
    t.testhv(...)
}
