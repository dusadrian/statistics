daria <- function(area, z1, z2, draw = FALSE, format = NULL) {
    .distribution_area(area, z1, z2, draw, format, pnorm, dnorm, 4)
}

.distribution_area <- function(area, first, second, draw, format, cdf, density, limit) {
    if (missing(area) || !is.character(area) || length(area) != 1L || is.na(area)) {
        stop("Supply a tail or interval name.", call. = FALSE)
    }
    area <- match.arg(area, c("right", "over", "above", "left", "under", "between"))
    .check_flag(draw, "draw")
    .check_flag(format, "format", allow_null = TRUE)
    if (missing(first) || !is.numeric(first) || length(first) != 1L || is.na(first)) {
        stop("Supply one numeric cutoff.", call. = FALSE)
    }
    if (area == "between") {
        if (missing(second) || !is.numeric(second) || length(second) != 1L || is.na(second)) {
            stop("Supply two numeric cutoffs for an interval.", call. = FALSE)
        }
        bounds <- sort(c(first, second))
        p <- if (bounds[1L] >= 0) {
            cdf(bounds[1L], lower.tail = FALSE) - cdf(bounds[2L], lower.tail = FALSE)
        } else {
            cdf(bounds[2L]) - cdf(bounds[1L])
        }
        cuts <- bounds
    } else if (area %in% c("right", "over", "above")) {
        p <- cdf(first, lower.tail = FALSE)
        bounds <- c(first, Inf)
        cuts <- first
    } else {
        p <- cdf(first)
        bounds <- c(-Inf, first)
        cuts <- first
    }

    if (draw) {
        if (dev.cur() == 1L) dev.new(width = 30/2.54, height = 21/2.54)
        old <- par(mar = c(1, 0, 0, 0))
        on.exit(par(old), add = TRUE)
        x <- seq(-limit, limit, length.out = 1001L)
        y <- density(x)
        plot(x, y, type = "l", axes = FALSE, xlab = "", ylab = "", ylim = c(0, max(y)))
        bounds <- pmax(-limit, pmin(limit, bounds))
        shaded <- seq(bounds[1L], bounds[2L], length.out = 501L)
        polygon(c(shaded[1L], shaded, shaded[length(shaded)]),
            c(0, density(shaded), 0), col = "#79a74c", border = NA)
        cuts <- cuts[abs(cuts) <= limit]
        if (length(cuts)) segments(cuts, 0, cuts, density(cuts))
        ticks <- seq(-limit, limit)
        ide <- grepl("RStudio|Positron", names(dev.cur()))
        axis(1, at = ticks, line = if (ide) -1 else -1.2, lwd = 0,
            cex.axis = if (ide) 0.8 else 1)
        segments(ticks, 0, ticks, -0.015 * diff(par("usr")[3:4]))
        segments(-limit, 0, limit, 0)
    }
    .probability_result(p, format)
}
