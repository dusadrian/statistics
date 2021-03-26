`unique_labelled` <- function(x) {
    
    labels <- attr(x, "labels")
    tagged <- logical(length(labels))
    
    if (is.double(labels)) {
        tagged <- haven::is_tagged_na(labels)
    }

    taglab <- labels[tagged]
    labels <- labels[!tagged]
    
    utag <- c()
    if (is.double(x)) {
        utag <- haven::na_tag(x)
        utag <- utag[!is.na(utag)]
        utag <- sort(unique(utag))
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

    if (is.double(x)) {
        x <- x[!haven::is_tagged_na(x)]
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

    xnotmis <- sort(remove_labels(x[!xmis]))
    xmis <- sort(remove_labels(x[xmis]))
    
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


`remove_labels` <- function(x) {
    y <- as.character(haven::as_factor(x, levels = "both"))
    natag <- haven::na_tag(x)
    
    if (any(is.na(y))) {
        # there are unlabelled tagged_na values
        y[is.na(y)] <- natag[is.na(y)]
    }
    

    labtag <- which(grepl("\\[NA\\]", y))

    if (length(labtag > 0)) {
        for (i in seq(length(labtag))) {
            y[labtag[i]] <- gsub("\\[NA\\]", natag[labtag[i]], y[labtag[i]])
        }
    }

    values <- gsub("\\[|\\]", "", unlist(lapply(strsplit(y, split = " "), "[[", 1)))
    
    numvals <- suppressWarnings(as.numeric(values))

    if (any(is.na(numvals))) {
        utags <- unique(values[is.na(numvals)])
        for (i in seq(length(utags))) {
            numvals[values == utags[i]] <- haven::tagged_na(utags[i])
        }
    }
    
    return(numvals)
}


`get_labels` <- function(x) {
    y <- as.character(haven::as_factor(x, levels = "both"))
    misvals <- c()
    
    natag <- haven::na_tag(x)
    
    if (any(is.na(y))) {
        # this means there are unlabelled tagged_na values
        y[is.na(y)] <- paste0(".", natag[is.na(y)])
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
    
    return(labels)
}
