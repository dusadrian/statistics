`dbinoms` <-
function(x, size, prob, log = FALSE, draw = FALSE,
        zoom = FALSE, new = FALSE, text = FALSE) {
    
    if (all(missing(x))) {
        cat("\n")
        stop("The number of favorable outcomes was not specified.\n\n", call. = FALSE)
    }
    
    if (!all(is.numeric(x))) {
        cat("\n")
        stop("The number of favorable outcomes should be a number or a numeric vector.\n\n", call. = FALSE)
    }
    
    if (missing(size)) {
        cat("\n")
        stop("The number of trials were not specified.\n\n", call. = FALSE)
        }
    
    if (!is.numeric(size) | size < 2) {
        cat("\n")
        stop("The number of trials is incorrect.\n\n", call. = FALSE)
        }
    
    if (missing(prob)) {
        cat("\n")
        stop("The probability of success was not specified.\n\n", call. = FALSE)
        }
    
    if (!is.numeric(prob) | prob < 0 | prob > 1) {
        cat("\n")
        stop("The probability of success should be a number between 0 and 1.\n\n", call. = FALSE)
        }
    
    if (!missing(size) & !all(missing(x))) {
        if (any(x > size)) {
            cat("\n")
            stop("The number of favorable outcomes cannot exceed the number of trials.\n\n", call. = FALSE)
        } else if (any(x < 0)) {
            cat("\n")
            stop("The number of favorable outcomes cannot be negative.\n\n", call. = FALSE)
        }
    }
    
    y <- y2 <- dbinom(0:size, size, prob, log=log)
    vector.culori <- rep("white", length(y))
    
    if (!draw) {
        focus <- FALSE
    }
    else {
        axaX <- 0:size
        if (zoom) {
            preamici <- y < 0.0001
            y <- y[!preamici]
            vector.culori <- rep("white", length(y))
            axaX <- axaX[!preamici]
        }
        
        if (new) {
            dev.new(width=30/2.54, height=21/2.54)
        }
        
        bare <- barplot(y) # , space=0
        colorate <- is.element(axaX, x)
        vector.culori[colorate] <- "#79a74c" # green
        
        barplot(y, names.arg = axaX, ylim = c(0, max(y)*1.05), col = vector.culori, space=0)
        
        if (text) {
            text(bare[colorate], y[colorate], round(y[colorate], digits = 3), pos = 3, cex = 0.8)
        }
        
        suma <- round(sum(y2[is.element(seq(0, size), x)]), digits = 3)
        
        # text(rep(max(bare)*ifelse(prob < 0.4, 0.8, 0.2), 4), max(y)*seq(0.95, 0.8, -0.05),
        #     c("The probability is", suma, "or", paste(suma*100, "%", sep = "")),
        #     col = rep(c("black", "#cb2626"), 2))
        
    }
    
    return(sum(y2[is.element(seq(0, size), x)]))
}

