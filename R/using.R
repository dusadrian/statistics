# http://adv-r.had.co.nz/Computing-on-the-language.html

`using` <- function(data, expr, split.by = NULL, ...) {

    expr <- substitute(expr)
    # select <- substitute(select)
    split.by <- as.character(substitute(split.by))
    
    # if (!is.null(select)) {
    #     data <- data[eval(expr = select, envir = data, enclos = parent.frame()), , drop = FALSE]
    # }

    
    if (length(split.by) > 1) {
        # if landing here, it means the split.by argument has more than one column
        # and this cannot possibly happen unless there is some way (other than using a comma)
        # to specify this, such as: c(A, B), or A & B, or A + B or something similar

        if (is.element(split.by[1], c("c", "&", "+"))) {
            split.by <- split.by[-1]
        }
        else {
            # for any other situations such as A ~ B
            cat("\n")
            stop(simpleError("Incorrect specification of the split.by argument.\n\n"))
        }
    }
    else if (length(split.by) == 1 & is.character(split.by)) {
        split.by <- admisc::splitstr(split.by)
    }

    if (is.null(split.by) || length(split.by) == 0) {
        return(eval(expr = expr, envir = data, enclos = parent.frame()))
    }

    if (!all(is.element(split.by, colnames(data)))) {
        cat("\n")
        stop(simpleError("One or more split.by variables not found in the data.\n\n"))
    }


    # split by levels
    sl <- lapply(split.by, function(sb) {
        x <- data[[sb]]

        if (is.factor(x)) {
            return(levels(x))
        }
        else if (is.element("haven_labelled", class(x))) {
            return(unique_labelled(x))
        }
        else if (!is.factor(x)) {
            cat("\n")
            stop(simpleError(sprintf("The split.by variable %s should be a factor or a labelled variable.\n\n", sb)))
        }

    })

    names(sl) <- split.by
    slexp <- expand.grid(sl)
    res <- vector(mode = "list", length = nrow(slexp))

    for (r in seq(nrow(slexp))) {
        selection <- logical(nrow(data))
        for (i in seq(ncol(slexp))) {

            val <- slexp[r, i]
            splitvar <- data[, split.by[i]]

            if (is.element("haven_labelled", class(splitvar))) {
                splitvar <- suppressMessages(labelled::remove_labels(splitvar))
                if (is.double(splitvar)) {
                    xtag <- haven::is_tagged_na(splitvar)
                    ntag <- haven::na_tag(splitvar)
                    splitvar[xtag] <- paste0(".", ntag[xtag])
                }
            }

            if (haven::is_tagged_na(val)) {
                val <- paste0(".", haven::na_tag(val))
            }

            selection <- selection | splitvar == val
        }

        cdata <- subset(data, selection)

        res[[r]] <- eval(expr = expr, envir = cdata, enclos = parent.frame())
    }


    class(res) <- "usage"
    attr(res, "split") <- expand.grid(lapply(sl, function(x) {

        if (!is.null(names(x))) {
            if (length(we <- which(names(x) == "")) > 0) {
                xtag <- logical(length(x))
                ntag <- x

                if (is.double(x)) { # condition to have tagged_na values
                    xtag <- haven::is_tagged_na(x)
                    ntag <- haven::na_tag(x)
                }

                for (i in seq(length(we))) {
                    if (xtag[we[i]]) {
                        names(x)[we[i]] <- paste0(".", ntag[we[i]])
                    }
                    else {
                        names(x)[we[i]] <- x
                    }
                }
            }

            return(names(x))
        }
        else {
            if (is.double(x)) { # condition to have tagged_na values
                xtag <- haven::is_tagged_na(x)
                ntag <- haven::na_tag(x)
            }

            if (inherits(x, "haven_labelled")) {
                return(names(val_labels(x)))
            }
            
            return(x)
        }
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
        cat("-----\n")
        print(x[[i]])
        if (i < length(x)) {
            cat("\n")
        }
    }
}
