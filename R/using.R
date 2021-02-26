# http://adv-r.had.co.nz/Computing-on-the-language.html

`using` <- function(data, expr, select = NULL, split.by = NULL, ...) {

    expr <- substitute(expr)
    split.by <- as.character(substitute(split.by))
    
    
    if (!is.null(select)) {
        data <- data[eval(expr = substitute(select), envir = data, enclos = parent.frame()), , drop = FALSE]
    }
    
    if (length(split.by) > 1) {
        split.by <- split.by[-1]
    }

    if (is.null(split.by) || length(split.by) == 0) {
        return(eval(expr = expr, envir = data, enclos = parent.frame()))
    }

    if (nchar(split.by[1]) == 1 & !is.element(split.by[1], colnames(data))) {
        # for situations such as A + B, or A ~ B, instead of the formal A & B
        cat("\n")
        stop(simpleError("Incorrect specification of the split.by argument.\n\n"))
    }

    if (!all(is.element(split.by, colnames(data)))) {
        cat("\n")
        stop(simpleError("One or more by variables not found in the data.\n\n"))
    }


    # split by levels
    sl <- lapply(split.by, function(sb) {
        x <- data[[sb]]

        if (is.element("haven_labelled", class(x))) {
            vallab <- attr(x, "labels")
            vallab <- vallab[!is.element(vallab, attr(x, "na_values"))]
            
            if (!is.null(na_range <- attr(x, "na_range"))) {
                vallab <- vallab[vallab < na_range[1] | vallab > na_range[2]]
            }
            
            return(vallab)
        }

        if (!is.factor(x)) {
            cat("\n")
            stop(simpleError(sprintf("The split variable %s should be a factor.\n\n", sb)))
        }

        return(levels(x))
    })

    names(sl) <- split.by

    res <- apply(expand.grid(sl), 1, function(x) {

        if (identical(as.character(expr)[[1]], "frtable")) {
            varx <- as.character(expr[[2]])
            if (!is.element(varx, names(data))) {
                cat("\n")
                stop(simpleError(sprintf("Variable %s not found in the data.\n\n", x)))
            }

            variable <- data[[varx]]
            if (is.atomic(variable) & !is.factor(variable) & is.element("haven_labelled", class(variable))) {

                vallab <- attr(variable, "labels")
                vallab <- vallab[!is.element(vallab, attr(variable, "na_values"))]
                na_range <- attr(variable, "na_range")
        
                if (!is.null(na_range)) {
                    misvals <- vallab[vallab >= na_range[1] & vallab <= na_range[2]]
                }

                vallab <- vallab[!is.element(vallab, misvals)]
                
                selection <- logical(length(variable))
                for (i in seq(length(x))) {
                    selection <- selection | data[, split.by[i]] == x[i]
                }
                
                variable <- variable[selection]
                misvals <- misvals[is.element(misvals, unique(variable))]

                # back tick and forward tick
                bftick <- paste(unlist(strsplit(rawToChar(as.raw(c(194, 180, 96))), split = "")), collapse = "|")

                names(vallab) <- gsub(bftick, "'", names(vallab))
                names(misvals) <- gsub(bftick, "'", names(misvals))
                vallab <- c(vallab, misvals)
                variable <- factor(variable, levels = vallab, labels = names(vallab), ordered = TRUE)
                attr(variable, "missing") <- misvals
                return(frtable(variable))
            }
        }

        tsubset <- paste("subset(data,", paste(split.by, paste("\"", x, "\"", sep = ""), sep = " == ", collapse = " & "), ")")
        cdata <- eval(parse(text = tsubset))
        
        eval(expr = expr, envir = cdata, enclos = parent.frame())
    })

    # return(res) 
    
    class(res) <- c("usage")
    attr(res, "split") <- expand.grid(lapply(sl, function(x) {
        if (!is.null(names(x))) return(names(x))
        return(x)
    }))
    return(res)
}


`print.usage` <- function(x, ...) {
    # nms <- apply(sl, 1, function(x) paste(names(x), x, sep = ":", collapse = "; "))
    
    nms <- apply(attr(x, "split"), 1, function(x) {
        paste(x, collapse = ", ")
    })

    for (i in seq(length(x))) {
        cat(nms[i], "\n")
        print(x[[i]])
        if (i < length(x)) {
            cat("-----\n")
        }
    }
}
