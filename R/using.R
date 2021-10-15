# http://adv-r.had.co.nz/Computing-on-the-language.html
# TODO: make use of the function split() to split by groups...!!

`using` <- function(data, expr, split.by = NULL, ...) {

    expr <- substitute(expr)
    # select <- substitute(select)
    split.by <- substitute(split.by)
    sby <- as.character(split.by)
    
    # if (!is.null(select)) {
    #     data <- data[eval(expr = select, envir = data, enclos = parent.frame()), , drop = FALSE]
    # }

    
    if (length(sby) > 1) {
        # if landing here, it means the split.by argument has more than one column
        # and this cannot possibly happen unless there is some way (other than using a comma)
        # to specify this, such as: c(A, B), or A & B, or A + B or something similar

        if (is.element(sby[1], c("c", "&", "+"))) {
            sby <- sby[-1]
        }
        else {
            # for any other situations such as A ~ B
            admisc::stopError("Incorrect specification of the argument <split.by>.")
        }
    }
    else if (length(sby) == 1 & is.character(sby)) {
        sby <- admisc::splitstr(sby)
    }

    if (is.null(sby) || length(sby) == 0) {
        return(eval(expr = expr, envir = data, enclos = parent.frame()))
    }

    if (!all(is.element(sby, colnames(data)))) {
        admisc::stopError("One or more split.by variables not found in the data.")
    }
    
    # split by levels
    sl <- lapply(sby, function(sb) {
        x <- data[[sb]]

        if (inherits(x, "haven_labelled")) {
            x <- declared::as_declared(x)
        }

        if (inherits(x, "declared")) {
            x <- sort(unique(x)) # that gets rid of the NAs because of sort()
            labels <- value_labels(x)
            labels <- labels[is.element(labels, x)]
            attributes(x) <- NULL
            names(x) <- x
            names(x)[match(labels, x)] <- names(labels)
            return(names(x))
        }
        
        if (is.factor(x)) {
            return(levels(x))
        }
        else {
            admisc::stopError(
                sprintf(
                    "The split.by variable %s should be a factor or a declared / labelled variable.",
                    sb
                )
            )
        }
        
    })


    names(sl) <- sby

    noflevels <- unlist(lapply(sl, length))
    mbase <- c(rev(cumprod(rev(noflevels))), 1)[-1]
    
    orep  <- cumprod(
        rev(
            c(rev(noflevels)[-1], 1)
        )
    )

    retmat <- sapply(seq_len(length(sl)), function(x) {
        rep.int(
            rep.int(
                seq_len(noflevels[x]) - 1,
                rep.int(mbase[x], noflevels[x])
            ),
            orep[x]
        ) + 1
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
            splitvar <- data[[sby[c]]]

            if (inherits(splitvar, "haven_labelled")) {
                splitvar <- declared::as_declared(splitvar)     
            }

            if (inherits(splitvar, "declared")) {
                splitvar <- declared::to_labels(splitvar)
            }

            selection <- selection & (splitvar == val)
        }

        if (sum(selection) > 0) {
            res[[r]] <- eval(
                expr = expr,
                envir = subset(data, selection),
                enclos = parent.frame()
            )
            # res[[r]] <- with(subset(data, selection), eval(expr))
        }
    }

    if (all(unlist(lapply(res, is.atomic)))) {

        # all are vectors (e.g. from summary) but lengths can differ
        # if one subset has NAs and others not
        lengths <- unlist(lapply(res, length))
        result <- matrix(NA, nrow = length(res), ncol = max(lengths))
        
        for (i in seq(length(res))) {
            if (!is.null(res[[i]])) {
                result[i, seq(length(res[[i]]))] <- res[[i]]
            }
        }
        
        for (i in seq(ncol(slexp))) {
            slexp[, i] <- format(slexp[, i], justify = "right")
        }
        rownames(result) <- apply(slexp, 1, function(x) paste(x, collapse = ", "))

        if (max(lengths) == 1) {
            colnames(result) <- as.character(as.list(expr)[[1]])
        }
        else {
            colnames(result) <- names(res[[which.max(lengths)]])
        }
        res <- result

    }
    else {
        attr(res, "split") <- slexp
    }
    
    class(res) <- "usage"
    return(res)
}


`print.usage` <- function(x, ...) {
    # nms <- apply(sl, 1, function(x) paste(names(x), x, sep = ":", collapse = "; "))
    if (is.list(x)) {
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
    else if (is.matrix(x)) {
        class(x) <- setdiff(class(x), "usage")
        
        if (ncol(x) == 1) {
            x[] <- prettyNum(round(x, 3))
        }
        else if (ncol(x) > 1) {
            # for instance summary() has an additional column to count NAs
            # but not all groups might have NAs when using split.by
            x[] <- gsub("NA", "", prettyNum(round(x, 3)))
        }
        
        for (i in seq(ncol(x))) {
            splitcol <- strsplit(x[, i], split = "[.]")
            nchars <- unlist(lapply(lapply(splitcol, "[", 2), nchar))
            if (!all(is.na(nchars))) {
                maxnchar <- max(nchars, na.rm = TRUE)
                x[, i] <- unlist(lapply(splitcol, function(x) {
                    if (length(x) == 1) {
                        x <- c(x, paste(c(rep("0", maxnchar)), collapse = ""))
                    }
                    else if (length(x) == 2) {
                        x[2] <- paste(x[2], paste(rep("0", maxnchar - nchar(x[2])), collapse = ""), sep = "")
                    }
                    return(paste(x, collapse = "."))
                }))
            }
        }
        
        colnames(x) <- format(colnames(x), justify = "right")
        print(noquote(format(x, justify = "right", width = max(nchar(colnames(x))))))
    }
}
