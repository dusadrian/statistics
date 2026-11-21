sbinom <- function(x) {
    weights <- .discrete_weights(x)
    keep <- weights > 0
    values <- x[keep, 1L]
    weights <- weights[keep]
    values <- values - (min(values)/2 + max(values)/2)
    scale <- max(abs(values))
    if (scale == 0) return(0)
    values <- values / scale
    center <- sum(values * weights)
    scale * sqrt(sum((values - center)^2 * weights))
}
