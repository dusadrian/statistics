dariat <- function(area, t1, t2, df, draw = FALSE, format = NULL) {
    if (missing(df) || !is.numeric(df) || length(df) != 1L || is.na(df) || df <= 0) {
        stop("Supply positive degrees of freedom.", call. = FALSE)
    }
    .distribution_area(area, t1, t2, draw, format,
        function(x, lower.tail = TRUE) pt(x, df, lower.tail = lower.tail),
        function(x) dt(x, df), if (df < 21) 5 else 4)
}
