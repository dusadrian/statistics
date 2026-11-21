histc <- function(x, from, to, size = 15, ...) {
    xname <- .expr_name(substitute(x))
    if (!is.numeric(x) || !is.null(dim(x))) stop("Supply a numeric vector.")
    x <- x[is.finite(x)]
    if (!length(x)) stop("Supply at least one finite observation.")
    args <- list(...)
    if (length(args)) {
        # Match positional and abbreviated arguments just as hist() would,
        # before separating the binning arguments from the plotting arguments.
        positions <- as.list(seq_along(args))
        names(positions) <- names(args)
        hist_call <- as.call(c(quote(hist), list(x = quote(x)), positions))
        matched <- as.list(match.call(graphics::hist.default, hist_call))[-c(1L, 2L)]
        names(args)[unlist(matched, use.names = FALSE)] <- names(matched)
    }
    calculation <- c("breaks", "include.lowest", "right", "nclass", "fuzz", "warn.unused")
    bins <- args[intersect(names(args), calculation)]
    args[calculation] <- NULL
    if (!missing(from) || !missing(to)) {
        if (missing(from)) from <- min(x)
        if (missing(to)) to <- max(x)
        if (!is.numeric(from) || length(from) != 1L || !is.finite(from) ||
            !is.numeric(to) || length(to) != 1L || !is.finite(to) || from >= to) {
            stop("Supply finite axis limits with 'from' below 'to'.")
        }
        if (is.null(args$xlim)) args$xlim <- c(from, to)
        if (is.null(bins$breaks) && is.null(bins$nclass)) {
            span <- range(x, from, to)
            bins$breaks <- pretty(span, n = grDevices::nclass.Sturges(x), min.n = 1L)
        }
    }
    h <- do.call(hist, c(list(x = quote(x), plot = FALSE), bins))
    h$xname <- xname
    if (isFALSE(args$plot)) return(h)
    args$plot <- NULL

    freq <- args$freq
    if (!is.null(args$probability)) {
        .check_flag(args$probability, "probability")
        if (!is.null(freq) && isTRUE(freq == args$probability)) {
            stop("'freq' and 'probability' specify conflicting scales.")
        }
        freq <- !args$probability
    }
    if (is.null(freq)) freq <- h$equidist
    .check_flag(freq, "freq")
    args$freq <- freq
    args$probability <- NULL
    widths <- diff(h$breaks)
    grid <- seq(min(h$breaks), max(h$breaks), length.out = 201L)
    if (freq && !h$equidist) {
        # Duplicate interior edges so the curve changes count scale at each bin.
        intervals <- split(grid, findInterval(grid, h$breaks, all.inside = TRUE))
        pieces <- lapply(seq_along(widths), function(i) {
            sort(unique(c(h$breaks[c(i, i + 1L)], intervals[[as.character(i)]])))
        })
        grid <- unlist(pieces, use.names = FALSE)
        widths <- rep(widths, lengths(pieces))
    } else {
        widths <- widths[1L]
    }
    deviation <- sd(x)
    curve_y <- if (is.finite(deviation) && deviation > 0) dnorm(grid, mean(x), deviation) else numeric()
    if (freq) curve_y <- curve_y * length(x) * widths
    if (is.null(args$ylim)) {
        bars <- if (freq) h$counts else h$density
        args$ylim <- c(0, max(bars, curve_y) * 1.1)
    }
    if (is.null(args$labels)) args$labels <- TRUE
    if (dev.cur() == 1L) {
        if (!is.numeric(size) || length(size) != 1L || !is.finite(size) || size <= 0) {
            stop("Supply a positive device size in centimeters.")
        }
        dev.new(width = size/2.54, height = size/2.54)
    }
    if (!isTRUE(args$add)) {
        old <- par(mar = c(4, 4, 2, 0))
        on.exit(par(old), add = TRUE)
    }
    do.call(plot, c(list(x = quote(h)), args))
    if (length(curve_y)) lines(grid, curve_y, col = "darkblue", lwd = 2)
    invisible(h)
}
