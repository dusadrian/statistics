`fretab` <- function(x, values = FALSE) {
    
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
                    stop(simpleError("This is an interval type variable with too many values.\n\n"))
                }
            }
        }

        cat("\n")
        stop(simpleError("Unsuitable input.\n\n"))
    }

    misvals <- NULL
    vals <- NULL
    vallab <- NULL
    
    if (is.factor(x)) {
        values <- FALSE
        vallab <- levels(x)
    }
    else if (is.element("haven_labelled", cls)) {
        vallab <- unique_labelled(x)
        x <- factor(get_labels(x), levels = names(vallab))
        misvals <- attr(vallab, "missing")
    }

    tbl <- table(x)
    res <- data.frame(fre = as.vector(tbl))
    rownames(res) <- names(tbl)

    res$rel <- prop.table(res$fre)
    res$per <- res$rel * 100
    res$cpd <- cumsum(res$per)

    attr(res, "values") <- values
    attr(res, "vallab") <- vallab
    class(res) <- c("fretab", "data.frame")
    return(res)
}

`print.fretab` <- function(x, force = FALSE, ...) {
    
    values <- attr(x, "values")
    vallab <- attr(x, "vallab")
    misvals <- attr(vallab, "missing")

    if (is.double(vallab)) {
        navals <- haven::na_tag(vallab)
        if (any(!is.na(navals))) {
            vallab[!is.na(navals)] <- paste0(".", navals[!is.na(navals)])
        }
    }

    if (is.double(misvals)) {
        namis <- haven::na_tag(misvals)
        if (any(!is.na(namis))) {
            misvals[!is.na(namis)] <- paste0(".", namis[!is.na(namis)])
        }
    }
    
    rnms <- rownames(x)
    if (values) {
        names(vallab)[unname(vallab) == names(vallab)] <- ""
        rnms <- paste(names(vallab), vallab, sep = " ")
        vallab <- vallab[is.element(vallab, misvals)]
        if (length(intersect(vallab, misvals)) > 0) {
            names(misvals)[is.element(misvals, vallab)] <- names(vallab)[is.element(vallab, misvals)]
        }

        if (!is.null(names(misvals))) {
            misvals <- paste(names(misvals), misvals, sep = " ")
        }
    }
    
    max.nchar.cases <- max(nchar(encodeString(rnms)))
    rnms <- sprintf(paste("% ", max.nchar.cases, "s", sep = ""), rnms)

    if (is.null(names(misvals))) {
        names(misvals) <- misvals
    }
    misvals <- sprintf(paste("% ", max.nchar.cases, "s", sep = ""), names(misvals))

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
    .Deprecated(msg = "Function frtable() is deprecated, and has been renamed to fretab()\n")
    fretab(...)
}


#
