`fretab` <- function(x) {
    cls <- intersect(class(x), c("numeric", "integer", "factor", "haven_labelled"))
    if (length(cls) == 0 | !is.atomic(x)) {
        if (is.atomic(x)) {
            if (is.character(x)) {
                cat("\n")
                stop(simpleError("The input is general text, should be converted to a factor.\n\n"))
            }

            if (is.numeric(x) | is.integer(x)) {
                if (length(unique(x)) > 15) {
                    cat("\n")
                    stop(simpleError("This is a interval type variable with too many values.\n\n"))
                }
            }
        }

        cat("\n")
        stop(simpleError("Unsuitable input.\n\n"))
    }

    misvals <- attr(x, "missing")
    
    if (!is.factor(x) & is.element("haven_labelled", cls)) {
        vallab <- sort(attr(x, "labels"))
        misvals <- attr(x, "na_values")
        na_range <- attr(x, "na_range")
        
        if (!is.null(na_range)) {
            misvals <- vallab[vallab >= na_range[1] & vallab <= na_range[2]]
        }

        xu <- sort(union(vallab, as.vector(unique(x))))
        names(xu) <- xu
        names(xu)[is.element(xu, vallab)] <- names(vallab)
        vallab <- xu

        if (!is.null(misvals)) {
            misvals <- vallab[is.element(vallab, misvals)]
            vallab <- vallab[!is.element(vallab, misvals)]
            misvals <- misvals[is.element(misvals, unique(x))]
            
            # back tick and forward tick
            bftick <- paste(unlist(strsplit(rawToChar(as.raw(c(194, 180, 96))), split = "")), collapse = "|")
            
            names(vallab) <- gsub(bftick, "'", names(vallab))
            names(misvals) <- gsub(bftick, "'", names(misvals))
            vallab <- c(vallab, misvals)
        }
        x <- factor(x, levels = vallab, labels = names(vallab), ordered = TRUE)
    }

    tbl <- table(x)
    res <- data.frame(fre = as.vector(tbl))
    rownames(res) <- names(tbl)

    res$rel <- prop.table(res$fre)
    res$per <- res$rel * 100
    res$cpd <- cumsum(res$per)

    attr(res, "missing") <- misvals
    class(res) <- c("fretab", "data.frame")
    return(res)
}

`print.fretab` <- function(x, force = FALSE, ...) {
    
    max.nchar.cases <- max(nchar(encodeString(rownames(x))))
    rnms <- sprintf(paste("% ", max.nchar.cases, "s", sep = ""), rownames(x))
    misvals <- sprintf(paste("% ", max.nchar.cases, "s", sep = ""), names(attr(x, "missing")))
    sums <- colSums(x[, 1:3])

    fres <- formatC(as.character(c(x$fre, sums[1])), format = "s")
    fres <- paste(sprintf(paste("%", max(3, nchar(sums[1])), "s", sep = ""), fres), "")
    # fres <- format(c(paste(rep(1, max(4, nchar(sums[1]))), collapse = ""), fres), justify = "centre")[-1]
    x$rel <- formatC(x$rel, digits = 3, format = "f")
    rel <- sprintf("% 5s", x$rel)
    x$per <- formatC(x$per, digits = 1, format = "f")
    per <- sprintf("% 5s", c(x$per, sums[3]))
    cpd <- formatC(x$cpd, digits = 1, format = "f")
    cpd <- sprintf(paste("% 5s", sep = ""), cpd)
    
    miseparator <- paste(c(rep(" ", ifelse(max.nchar.cases > 5, max.nchar.cases - 5, 0)),
                           rep("-", min(max.nchar.cases, 5) + 1 * (sums[1] >= 1000)), "\n"), collapse = "")
    separator <- paste(c(rep(" ", max.nchar.cases + 1), rep("-", nchar(sums[1])),
        ifelse(nchar(sums[1]) < 3, paste(rep("-", 3 - nchar(sums[1])), collapse = ""), ""),
        "-------------------\n"), collapse = "")

    if (nrow(x) > 100 & !force) {
        cat("\n")
        stop(simpleError("Hmm... it looks like having lot of categories. If you really want to print it, use:\nprint(x, force = TRUE)\n\n"))
    }

    cat(paste(rep(" ", max.nchar.cases + ifelse(nchar(sums[1]) > 4, nchar(sums[1]) - 4, 0)), collapse = ""),
        ifelse(sums[1] < 1000, "fre    rel   per   cpd\n", " fre    rel   per   cpd\n"))
    cat(separator)
    for (i in seq(nrow(x))) {
        if (is.element(rnms[i], misvals)) {
            cat(miseparator)
            misvals <- NULL
        }
        cat(rnms[i], fres[i], rel[i], per[i], cpd[i], "\n")
    }
    cat(separator)
    # cat(paste(rep(" ", max.nchar.cases), sep = ""), " ", sprintf(paste("% ", max(4, nchar(sums[1])), "s", sep = ""), sums[1]), " 1.000 100.0\n", sep = "")
    cat(paste(rep(" ", max.nchar.cases), sep = ""), " ", fres[length(fres)], " 1.000 100.0\n", sep = "")

}




`frtable` <- function(...) {
    .Deprecated(msg = "Function frtable() is deprecated, and has been renamed to fret()\n")
    fret(...)
}
