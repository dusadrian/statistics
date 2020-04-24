`frtable` <- function(x) {
    cls <- intersect(class(x), c("numeric", "integer", "factor", "haven_labelled"))
    if (length(cls) == 0 | !is.atomic(x)) {
        cat("\n")
        stop(simpleError("Unsuitable input.\n\n"))
    }

    misvals <- attr(x, "missing")
    
    if (!is.factor(x) & is.element("haven_labelled", cls)) {
        vallab <- attr(x, "labels")
        misvals <- attr(x, "na_values")
        na_range <- attr(x, "na_range")
        
        if (!is.null(na_range)) {
            misvals <- vallab[vallab >= na_range[1] & vallab <= na_range[2]]
        }

        vallab <- vallab[!is.element(vallab, misvals)]
        misvals <- misvals[is.element(misvals, unique(x))]
        names(vallab) <- gsub("´|`", "'", names(vallab))
        names(misvals) <- gsub("´|`", "'", names(misvals))
        vallab <- c(vallab, misvals)
        x <- factor(x, levels = vallab, labels = names(vallab), ordered = TRUE)
    }


    tbl <- table(x)
    res <- data.frame(freq = as.vector(tbl))
    rownames(res) <- names(tbl)

    res$relf <- prop.table(res$freq)
    res$perc <- res$relf * 100
    res$cump <- cumsum(res$perc)

    attr(res, "missing") <- misvals
    class(res) <- c("frtable", "data.frame")
    return(res)
}

`print.frtable` <- function(x, force = FALSE, ...) {
    
    max.nchar.cases <- max(nchar(encodeString(rownames(x))))
    rnms <- sprintf(paste("% ", max.nchar.cases, "s", sep = ""), rownames(x))
    misvals <- sprintf(paste("% ", max.nchar.cases, "s", sep = ""), names(attr(x, "missing")))
    sums <- colSums(x[, 1:3])

    freqs <- formatC(as.character(c(x$freq, sums[1])), format = "s")
    freqs <- sprintf(paste("% ", max(4, nchar(sums[1])), "s", sep = ""), freqs)
    x$relf <- formatC(x$relf, digits = 3, format = "f")
    relf <- sprintf("% 5s", x$relf)
    x$perc <- formatC(x$perc, digits = 1, format = "f")
    perc <- sprintf("% 5s", c(x$perc, sums[3]))
    cump <- formatC(x$cump, digits = 1, format = "f")
    cump <- sprintf(paste("% 5s", sep = ""), cump)
    
    miseparator <- paste(c(rep(" ", ifelse(max.nchar.cases > 5, max.nchar.cases - 5, 0)),
                           rep("-", min(max.nchar.cases, 5)), "\n"), collapse = "")
    separator <- paste(c(rep(" ", max.nchar.cases + 1), rep("-", nchar(sums[1])),
        ifelse(nchar(sums[1]) < 4, paste(rep("-", 4 - nchar(sums[1])), collapse = ""), ""),
        "------------------\n"), collapse = "")

    if (nrow(x) > 100 & !force) {
        cat("\n")
        stop(simpleError("Hmm... this looks like a lot of categories. If you really want to print it, use:\nprint(x, force = TRUE)\n\n"))
    }

    cat(paste(rep(" ", max.nchar.cases + ifelse(nchar(sums[1]) > 4, nchar(sums[1]) - 4, 0)), collapse = ""), "freq  relf  perc  cump\n")
    cat(separator)
    for (i in seq(nrow(x))) {
        if (is.element(rnms[i], misvals)) {
            cat(miseparator)
            misvals <- NULL
        }
        cat(rnms[i], freqs[i], relf[i], perc[i], cump[i], "\n")
    }
    cat(separator)
    cat(paste(rep(" ", max.nchar.cases), sep = ""), " ", sprintf(paste("% ", max(4, nchar(sums[1])), "s", sep = ""), sums[1]), " 1.000 100.0\n", sep = "")

}