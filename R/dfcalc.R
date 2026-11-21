.sample_sd <- function(x) {
    s <- sd(x)
    if (is.na(s) || (s > 0 && is.finite(s))) return(s)
    scale <- max(abs(x))
    if (scale == 0 || !is.finite(scale)) return(s)
    sd(x / scale) * scale
}

.sample_summaries <- function(x, y, n1, n2) {
    if (missing(x) || missing(y)) stop("Supply both samples or both standard deviations.")
    if (missing(n1) && missing(n2)) {
        if (!is.numeric(x) || !is.numeric(y) || !is.null(dim(x)) || !is.null(dim(y)) ||
            min(length(x), length(y)) < 2L) stop("Supply two numeric samples of at least two values.")
        return(list(s = c(.sample_sd(x), .sample_sd(y)), n = as.double(c(length(x), length(y)))))
    }
    if (missing(n1) || missing(n2)) stop("Supply both sample sizes.")
    if (!is.numeric(n1) || !is.numeric(n2) || length(n1) != 1L || length(n2) != 1L) {
        stop("Supply one sample size per group.")
    }
    n <- as.double(c(n1, n2))
    if (any(!is.finite(n) | n < 2 | n != floor(n))) stop("Sample sizes must be integers of at least two.")
    if (!is.numeric(x) || !is.numeric(y) || length(x) != 1L || length(y) != 1L ||
        any(!is.finite(c(x, y)) | c(x, y) < 0)) stop("Supply two finite, nonnegative standard deviations.")
    list(s = c(x, y), n = n)
}

dfcalc <- function(x, y, n1, n2) {
    groups <- .sample_summaries(x, y, n1, n2)
    se <- groups$s / sqrt(groups$n)
    if (anyNA(se)) return(NA_real_)
    if (max(se) == 0) return(NaN)
    weights <- (se / max(se))^2
    1 / sum((weights / sum(weights))^2 / (groups$n - 1))
}
