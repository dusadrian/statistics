dbinoms <- function(x, size, prob, log = FALSE, draw = FALSE,
        zoom = FALSE, new = FALSE, text = FALSE) {
    if (!is.numeric(size) || length(size) != 1L || !is.finite(size) ||
        size < 0 || size != floor(size)) stop("Supply a nonnegative integer trial count.")
    if (!is.numeric(prob) || length(prob) != 1L || !is.finite(prob) ||
        prob < 0 || prob > 1) stop("Supply a probability between zero and one.")
    if (!is.numeric(x) || any(!is.finite(x) | x < 0 | x > size | x != floor(x))) {
        stop("Outcomes must be whole numbers between zero and the trial count.")
    }
    .check_flag(log, "log")
    .check_flag(draw, "draw")
    .check_flag(zoom, "zoom")
    .check_flag(new, "new")
    .check_flag(text, "text")
    x <- unique(x)
    mass <- dbinom(x, size, prob, log = log)
    if (!log) {
        result <- min(1, sum(mass))
    } else if (!length(mass) || all(mass == -Inf)) {
        result <- -Inf
    } else {
        peak <- max(mass)
        result <- min(0, peak + log(sum(exp(mass - peak))))
    }
    if (draw) {
        outcomes <- 0:size
        heights <- dbinom(outcomes, size, prob)
        if (zoom) {
            keep <- heights > 0
            outcomes <- outcomes[keep]
            heights <- heights[keep]
        }
        if (new) dev.new(width = 30/2.54, height = 21/2.54)
        selected <- outcomes %in% x
        centers <- barplot(heights, names.arg = outcomes, ylim = c(0, max(heights) * 1.1),
            col = ifelse(selected, "#79a74c", "white"), space = 0)
        if (text && any(selected)) {
            text(centers[selected], heights[selected],
                vapply(heights[selected], .format_probability, character(1)), pos = 3, cex = 0.8)
        }
    }
    if (log) result else .probability_result(result)
}
