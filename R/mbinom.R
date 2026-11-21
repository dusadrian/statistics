.discrete_weights <- function(x) {
    if (!(is.matrix(x) || is.data.frame(x)) || ncol(x) != 2L || nrow(x) == 0L) {
        stop("Supply a two-column table of outcomes and probabilities or frequencies.")
    }
    weights <- x[, 2L]
    if (!is.numeric(x[, 1L]) || any(!is.finite(x[, 1L]))) stop("Outcomes must be finite numbers.")
    if (!is.numeric(weights) || any(!is.finite(weights) | weights < 0) || !any(weights > 0)) {
        stop("Weights must be finite and nonnegative with a positive total.")
    }
    weights <- weights / max(weights)
    weights / sum(weights)
}

mbinom <- function(x) {
    weights <- .discrete_weights(x)
    sum(x[, 1L] * weights)
}
