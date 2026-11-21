spooled <- function(x, y, n1, n2) {
    groups <- .sample_summaries(x, y, n1, n2)
    scale <- max(groups$s)
    if (is.na(scale) || scale == 0) return(scale)
    df <- groups$n - 1
    df <- df / max(df)
    scale * sqrt(sum(df * (groups$s / scale)^2) / sum(df))
}
