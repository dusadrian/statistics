`sort_labelled` <- function(x) {

    if (!is.element("haven_labelled", class(x))) {
        cat("\n")
        stop("The input should be a labelled vector.\n\n", call. = FALSE)
    }

    attrx <- attributes(x)
    attributes(x) <- NULL

    tagged <- logical(length(x))

    tags <- c()
    if (is.double(x)) {
        tagged <- haven::is_tagged_na(x)
        if (any(tagged)) {
            tags <- haven::tagged_na(sort(haven::na_tag(x[tagged])))
        }
    }

    xmis <- logical(length(x))
    xmis <- xmis | (is.element(x, attrx$na_values[!is_tagged_na(attrx$na_values)]) & !tagged)

    na_range <- attrx$na_range

    if (!is.null(na_range)) {
        xmis <- xmis | ((x <= na_range[1] | x >= na_range[2]) & !tagged)
    }

    truena <- is.na(x) & !xmis & !tagged

    # > c(x[!xmis], x[xmis])
    # Error: C stack usage  7970768 is too close to the limit
    # therefore
    
    result <- c(sort(x[!xmis & !truena]), sort(x[xmis & !truena]), tags, x[truena])
    
    attributes(result) <- attrx
    return(result)

}

`unique_labelled` <- function(x, sort = FALSE) {

    if (!is.element("haven_labelled", class(x))) {
        cat("\n")
        stop("The input should be a labelled vector.\n\n", call. = FALSE)
    }

    attrx <- attributes(x)
    attributes(x) <- NULL

    tagged <- logical(length(x))

    if (is.double(x)) {
        tagged <- haven::is_tagged_na(x)
    }

    if (any(tagged)) {
        x[tagged] <- haven::na_tag(x[tagged])
    }

    dupx <- duplicated(x)
    tagged <- tagged[!dupx]
    x <- x[!dupx]

    result <- rep(NA, length(unique(x)))

    result[!(is.na(x) | tagged)] <- as.numeric(x[!(is.na(x) | tagged)])
    if (any(tagged)) {
        result[tagged] <- haven::tagged_na(x[tagged])
    }
    
    attributes(result) <- attrx
    return(result)
}


    


`names_values` <- function(x) {

    labels <- attr(x, "labels")
    tagged <- logical(length(labels))
    
    if (is.double(labels)) {
        tagged <- haven::is_tagged_na(labels)
    }

    nax <- is.na(x) & !tagged

    taglab <- labels[tagged]
    labels <- labels[!tagged]
    
    utag <- c()
    if (is.double(x)) {
        utag <- haven::na_tag(x)
        utag <- utag[!is.na(utag)]
        utag <- sort(unique(utag))
        x <- x[!haven::is_tagged_na(x)]
    }

    numtag <- c()

    if (length(utag > 0)) {
        numtag <- haven::tagged_na(utag)
        labtag <- c()

        if (length(taglab) > 0) {
            labtag <- haven::na_tag(taglab)
        }

        names(numtag) <- paste0(".", utag)
    
        for (i in seq(length(utag))) {
            if (any(isel <- labtag == utag[i])) {
                # only one can be true, impossible more
                names(numtag)[i] <- names(taglab)[isel]
            }
        }
    }

    x <- x[!duplicated(x)]
    xmis <- logical(length(x))

    na_values <- attr(x, "na_values")
    na_range <- attr(x, "na_range")

    if (!is.null(na_values)) {
        xmis <- xmis | is.element(x, na_values)
    }
    
    if (!is.null(na_range)) {
        xmis <- xmis | x <= na_range[1] | x >= na_range[2]
    }

    xnotmis <- suppressMessages(sort(labelled::remove_labels(x[!xmis])))
    xmis <- suppressMessages(sort(labelled::remove_labels(x[xmis])))
    
    if (length(xmis) > 0) {
        names(xmis) <- xmis
        for (i in seq(length(xmis))) {
            if (any(isel <- labels == xmis[i])) {
                names(xmis)[i] <- names(labels)[isel]
            }
        }
    }


    names(xnotmis) <- xnotmis
    for (i in seq(length(xnotmis))) {
        if (any(isel <- labels == xnotmis[i])) {
            names(xnotmis)[i] <- names(labels)[isel]
        }
    }

    result <- c(xnotmis, xmis, numtag)
    attr(result, 'missing') <- c(xmis, numtag)

    return(result)
}



`get_labels` <- function(x) {

    if (!is.element("haven_labelled", class(x))) {
        cat("\n")
        stop("The input should be a labelled vector.\n\n", call. = FALSE)
    }

    # as_factor() alone is not good, if there is a tagged_na() value that
    # does not have a label (but it is still a different missing value)

    # x
    # <labelled<double>[7]>
    # [1]  1  2  3  4  5 .a NA

    # Labels:
    # value label
    #     1  Good
    #     5   Bad

    # as.character(haven::as_factor(x, levels = "both"))
    # [1] "[1] Good" "2"        "3"        "4"        "[5] Bad"  NA         NA

    # while I am expecting:
    # [1] "[1] Good" "2"        "3"        "4"        "[5] Bad"  ".a"         NA


    tagged <- logical(length(x))

    if (is.double(x)) {
        tagged <- haven::is_tagged_na(x)
    }
    
    tags <- paste0(".", haven::na_tag(x[tagged]))
    
    y <- as.character(haven::as_factor(x, levels = "both"))


    misvals <- c()
    
    natag <- haven::na_tag(x)
    tagged <- logical(length(x))

    if (is.double(x)) {
        tagged <- haven::is_tagged_na(x)
    }


    labtag <- which(grepl("\\[NA\\]", y))

    if (length(labtag) > 0) {
        for (i in seq(length(labtag))) {
            y[labtag[i]] <- gsub("\\[NA\\]", natag[labtag[i]], y[labtag[i]])
        }
    }

    xlist <- strsplit(y, split = " ")
    values <- gsub("\\[|\\]", "", unlist(lapply(xlist, "[[", 1)))
    labels <- unlist(lapply(xlist, function(x) {
        if (length(x) == 1) {
            return(x)
        }

        return(paste(x[-1], collapse = " "))
    }))

    
    labels[is.na(labels) & tagged] <- tags
    
    return(labels)
}
