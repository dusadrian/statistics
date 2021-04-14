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
            return(to_labels(sort_labelled(unique_labelled(x))))
        }
        else if (!is.factor(x)) {
            cat("\n")
            stop(simpleError(sprintf("The split.by variable %s should be a factor or a labelled variable.\n\n", sb)))
        }
    })


    names(sl) <- split.by
    noflevels <- unlist(lapply(sl, length))
    mbase <- c(rev(cumprod(rev(noflevels))), 1)[-1]
    orep  <- cumprod(rev(c(rev(noflevels)[-1], 1)))
    retmat <- sapply(seq_len(length(sl)), function(x) {
        rep.int(rep.int(seq_len(noflevels[x]) - 1, rep.int(mbase[x], noflevels[x])), orep[x]) + 1
    })
    
    # slexp <- expand.grid(sl, stringsAsFactors = FALSE)

    slexp <- retmat
    for (i in seq(length(sl))) {
        slexp[, i] <- sl[[i]][retmat[, i]]
    }
    
    res <- vector(mode = "list", length = nrow(slexp))

    for (r in seq(nrow(slexp))) {
        selection <- rep(TRUE, nrow(data))

        for (c in seq(ncol(slexp))) {
            val <- slexp[r, c]
            splitvar <- data[[split.by[c]]]

            if (is.element("haven_labelled", class(splitvar))) {
                splitvar <- to_labels(splitvar)
            }

            selection <- selection & (splitvar == val)
            
        }

        if (sum(selection == 0)) {
            res[[r]] <- NULL
        }
        else {
            cdata <- subset(data, selection)
            res[[r]] <- eval(expr = expr, envir = cdata, enclos = parent.frame())
        }
    }

    

    class(res) <- "usage"
    attr(res, "split") <- slexp

    return(res)
}


`print.usage` <- function(x, ...) {
    # nms <- apply(sl, 1, function(x) paste(names(x), x, sep = ":", collapse = "; "))
    
    nms <- apply(attr(x, "split", exact = TRUE), 1, function(x) {
        paste(x, collapse = ", ")
    })

    for (i in seq(length(x))) {
        cat(nms[i], "\n")
        cat("-----\n")
        
        if (is.null(x[[i]])) {
            cat("No data.\n")
        }
        else {
            print(x[[i]])
        }
        
        if (i < length(x)) {
            cat("\n")
        }
    }
}
