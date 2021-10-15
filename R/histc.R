`histc` <-
function(x, from, to, size = 15, ...) {

    if (!is.atomic(x) || !is.numeric(x)) {
        cat("\n")
        stop(simpleError("`x` should be a numerical vector.\n\n"))
    }

    x <- unclass(x)
    x <- x[!is.na(x)]

    other.args <- list(...)
    funargs <- unlist(lapply(match.call(), deparse)[-1])
    if (dev.cur() == 1) {
        dev.new(width = (size + 1)/2.54, height = (size + 1)/2.54)
    }
    
    par(mar = c(4, 4, 2, 0))
    testh <- hist(x, plot = FALSE)
    bdiff <- diff(testh$breaks[1:2])
    
    xlab <- admisc::getName(funargs[1])
    maintitle <- paste("Histogram of", xlab)
    
    
    if (length(testarg <- which(names(other.args) == "xlab")) > 0) {
        xlab <- other.args$xlab
        other.args <- other.args[-testarg]
    }
    
    if (length(testarg <- which(names(other.args) == "main")) > 0) {
        maintitle <- other.args$main
        other.args <- other.args[-testarg]
    }
    
    tohist <- list(as.name("hist"), x, plot = FALSE)
    tohist <- c(tohist, other.args)
    
    if (!missing(from) & !missing(to)) {
        tohist$breaks <- seq(from, to, by = bdiff)
        tohist$xlim <- c(from, to)
        
        h <- suppressWarnings(eval(as.call(tohist)))
        
        tohist$ylim <- c(0, max(h$counts)*1.1)
        tohist$xlab <- xlab
        tohist$main <- maintitle
        tohist$axes <- FALSE
        
        tohist$plot <- NULL
        eval(as.call(tohist))
    }
    else {
        h <- suppressWarnings(eval(as.call(tohist)))
        tohist$ylim <- c(0, max(h$counts)*1.1)
        tohist$xlab <- xlab
        tohist$main <- maintitle
        tohist$axes <- FALSE
        tohist$plot <- NULL
        
        eval(as.call(tohist))
    }
    
    text(h$mids, h$counts, h$counts, pos = 3, cex = 0.9)
    xfit <- seq(ifelse(missing(from), min(h$breaks), from), ifelse(missing(to), max(h$breaks), to), length = 100)
    yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))*bdiff*length(x)
    lines(xfit, yfit, col = "darkblue", lwd = 2)
    if (!missing(from) & !missing(to)) {
        axis(1, at = seq(from, to, by = bdiff))
    }
    else {
        axis(1)
    }
    axis(2)
}
