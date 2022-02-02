`frtable` <- function(x, values = TRUE) {
    
    haven <- eval(parse(text = "requireNamespace('haven', quietly = TRUE)"))
    
    if (inherits(x, "haven_labelled")) {
        if (haven) {
            if (eval(parse(text = "any(haven::is_tagged_na(x))"))) {
                cat("\n")
                stop(simpleError("Tagged NAs are not supported.\n\n"))
            }
        }

        if (!inherits(x, "haven_labelled_spss")) {
            class(x) <- c("haven_labelled_spss", class(x))
        }

        x <- as_declared(x)
    }
    
    
    cls <- intersect(class(x), c("numeric", "integer", "factor", "declared"))
    
    if (length(cls) == 0 | !is.atomic(x)) {
        if (is.atomic(x)) {
            if (is.character(x)) {
                cat("\n")
                stop(simpleError("The input is general text, should be converted to a factor.\n\n"))
            }

            if (is.numeric(x) | is.integer(x)) {
                if (length(unique(x)) > 15) {
                    cat("\n")
                    stop(simpleError("This looks like an interval type variable with too many values.\n\n"))
                }
            }
        }

        cat("\n")
        stop(simpleError("Unsuitable input.\n\n"))
    }

    vals <- NULL
    vallab <- NULL
    
    if (inherits(x, "declared")) {
        vallab <- declared::names_values(x)
        x <- factor(declared::to_labels(x), levels = names(vallab))
    }
    else {
        values <- FALSE
        if (is.factor(x)) {
            vallab <- levels(x)
        }
    }

    tbl <- table(x)
    if (any(is.na(x))) {
        tbl <- c(tbl, sum(is.na(x)))
        names(tbl)[length(tbl)] <- "NA"
    }

    res <- data.frame(fre = as.vector(tbl))
    rownames(res) <- names(tbl)

    res$rel <- prop.table(res$fre)
    res$per <- res$rel * 100
    res$cpd <- cumsum(res$per)

    attr(res, "values") <- values
    attr(res, "vallab") <- vallab
    class(res) <- c("frtable", "data.frame")
    return(res)
}



`print.frtable` <- function(x, force = FALSE, ...) {
    
    values <- attr(x, "values")
    vallab <- attr(x, "vallab")
    misvals <- attr(vallab, "missing")
    
    irv <- c(194, 180)
    tick <- unlist(strsplit(rawToChar(as.raw(irv)), split = ""))
    
    rnms <- gsub(paste(tick, collapse = "|"), "'", rownames(x))
    first_missing <- 0
    
    if (values) {
        names(vallab)[unname(vallab) == names(vallab)] <- ""
        rnms <- paste(gsub(paste(tick, collapse = "|"), "'", names(vallab)), vallab, sep = " ")
        
        if (identical(rownames(x)[nrow(x)], "NA")) {
            rnms <- c(rnms, "NA")
        }

        vallab <- vallab[is.element(vallab, misvals)]
        if (length(intersect(vallab, misvals)) > 0) {
            names(misvals)[is.element(misvals, vallab)] <- names(vallab)[is.element(vallab, misvals)]
        }

        if (!is.null(names(misvals))) {
            misvals <- paste(names(misvals), misvals, sep = " ")
            misvals <- gsub(paste(tick, collapse = "|"), "'", misvals)
        }

        if (any(is.element(rnms, misvals))) {
            first_missing <- which(is.element(rnms, misvals))[1]
        }
    }
    
    
    max.nchar.cases <- max(nchar(encodeString(rnms)))
    # rnms <- sprintf(paste0("% ", max.nchar.cases, "s"), rnms)
    for (i in seq(length(rnms))) {
        if (nchar(rnms[i]) < max.nchar.cases) {
            rnms[i] <- paste(c(rep(" ", max.nchar.cases - nchar(rnms[i])), rnms[i]), collapse = "", sep = "")
        }
    }

    if (is.null(names(misvals))) {
        names(misvals) <- misvals
    }

    # if (identical(rownames(x)[nrow(x)], "NA")) {
    #     misvals <- c(misvals, "NA")
    # }

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
        stop(simpleError("It looks like a lot of categories. If you really want to print it, use:\nprint(x, force = TRUE)\n\n"))
    }

    cat(
        paste(
            rep(
                " ",
                max.nchar.cases + ifelse(nchar(sums[1]) > 4, nchar(sums[1]) - 4, 0)
            ),
            collapse = ""
        ),
        ifelse(
            sums[1] < 1000,
            "fre    rel   per   cpd\n",
            " fre    rel   per   cpd\n"
        )
    )

    cat(separator)

    for (i in seq(nrow(x))) {
        if (first_missing == i) {
            cat(miseparator)
        }
        cat(rnms[i], fres[i], rel[i], per[i], cpd[i], "\n")
    }

    cat(separator)

    # cat(paste(rep(" ", max.nchar.cases), sep = ""), " ", sprintf(paste("% ", max(4, nchar(sums[1])), "s", sep = ""), sums[1]), " 1.000 100.0\n", sep = "")
    cat(
        paste(
            rep(" ", max.nchar.cases),
            sep = ""
        ),
        " ",
        fres[length(fres)],
        " 1.000 100.0\n",
        sep = ""
    )

}
