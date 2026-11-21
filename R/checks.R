.check_flag <- function(x, name, allow_null = FALSE) {
    if (allow_null && is.null(x)) return(invisible())
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
        stop(sprintf("The '%s' argument should be TRUE or FALSE.", name), call. = FALSE)
    }
}

.check_dots <- function(...) {
    args <- substitute(list(...))
    if (length(args) > 1L) {
        stop("Unused argument(s): ", .expr_name(args), call. = FALSE)
    }
}

.conf_level <- function(x) {
    if (is.null(x)) x <- 0.95
    if (length(x) != 1L || !admisc::possibleNumeric(x)) {
        stop("Supply one numeric confidence level.", call. = FALSE)
    }
    x <- admisc::asNumeric(x)
    if (!is.finite(x) || x < 0 || x > 1) {
        stop("The confidence level should be between 0 and 1.", call. = FALSE)
    }
    x
}

.expr_name <- function(expr) paste(deparse(expr), collapse = " ")

.select_pooled <- function(var.equal, p, conf.level, welch_unavailable) {
    if (!is.null(var.equal)) return(var.equal)
    welch_unavailable || isTRUE(p > 1 - conf.level)
}
