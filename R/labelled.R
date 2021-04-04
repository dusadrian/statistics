`order_labelled` <- function(x, according_to = c("values", "labels"), decreasing = FALSE,
    user_na = c("last", "first", "ignore", "na"), na_value = c("last", "first", "na")) {

    if (!inherits(x, "haven_labelled")) {
        cat("\n")
        stop("The input should be a labelled vector.\n\n", call. = FALSE)
    }

    according_to <- match.arg(according_to)
    user_na <- match.arg(user_na)
    na_value = match.arg(na_value)

    labels <- labelled::val_labels(x)
    na_values <- labelled::na_values(x)
    na_range <- labelled::na_range(x)

    indexes <- seq(length(x))

    attrx <- attributes(x)
    attributes(x) <- NULL

    if (is.double(x)) {
        tagged <- haven::is_tagged_na(x)
    }
    else {
        tagged <- logical(length(x))
    }

    xmis <- logical(length(x)) # user defined missing values but not tagged
    xmis <- xmis | (is.element(x, na_values[!haven::is_tagged_na(na_values)]) & !tagged)

    if (!is.null(na_range)) {
        xmis <- xmis | ((x <= na_range[1] | x >= na_range[2]) & !tagged)
    }

    truena <- is.na(x) & !xmis & !tagged
    basena <- indexes[truena]
    tagged <- tagged[!truena]
    xmis <- xmis[!truena]
    indexes <- indexes[!truena]

    x <- x[!truena]
    
    result <- c()
    if (na_value == "first") {
        result <- basena
        length(basena) <- 0
    }

    y <- x

    if (is.double(labels)) {
        taglab <- haven::is_tagged_na(labels)
        labels[taglab] <- haven::na_tag(labels[taglab])
    }

    if (any(tagged)) {
        y[tagged] <- haven::na_tag(x[tagged])
    }

    if (according_to == "labels") {
        haslabels <- is.element(y, labels)
        tagged_no_label <- !haslabels & tagged

        if (any(tagged_no_label)) {
            if (length(result) > 0) {
                result <- c(result, indexes[tagged_no_label])
            }
            else {
                basena <- c(indexes[tagged_no_label], basena)
            }

            indexes <- indexes[!tagged_no_label]
            x <- x[!tagged_no_label]
            y <- y[!tagged_no_label]
            xmis <- xmis[!tagged_no_label]
            tagged <- tagged[!tagged_no_label]
        }
    }

    if (na_value == "na") {
        length(basena) <- 0
    }
    
    z <- y[xmis | tagged]
    if (according_to == "labels") {
        haslabels <- is.element(z, labels)
        z[haslabels] <- names(labels)[match(z[haslabels], labels)]
    }
    if (user_na == "first") {
        result <- c(result, indexes[xmis | tagged][order(z, decreasing = decreasing)])
    }
    else if (user_na == "last") {
        basena <- c(indexes[xmis | tagged][order(z, decreasing = decreasing)], basena)
    }
    
    if (user_na != "ignore") {
        indexes <- indexes[!xmis & !tagged]
        x <- x[!xmis & !tagged]
        y <- y[!xmis & !tagged]
    }
    

    if (according_to == "labels") {
        haslabels <- is.element(y, labels)
        y[haslabels] <- names(labels)[match(y[haslabels], labels)]
        result <- c(result, indexes[order(y, decreasing = decreasing)])
    }
    else {
        result <- c(result, indexes[order(y, decreasing = decreasing)])
    }

    result <- c(result, basena)
    return(result)

    attributes(result) <- attrx
    return(result)
}



`sort_labelled` <- function(x, according_to = c("values", "labels"), decreasing = FALSE,
    user_na = c("last", "first", "ignore", "na"), na_value = c("last", "first", "na")) {

    # vec_sort() in package vctrs is somewhat similar, but still does not
    # differentiate between (hence does not sort) different tagged_na values

    return(x[order_labelled(x, according_to = according_to, decreasing = decreasing, user_na = user_na, na_value = na_value)])
}



`unique_labelled` <- function(x, sort = FALSE, ...) {

    if (!inherits(x, "haven_labelled")) {
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
    
    if (sort) {
        return(sort_labelled(result, ... = ...))
    }

    return(result)
}



`names_values` <- function(x) {

    if (!inherits(x, "haven_labelled")) {
        cat("\n")
        stop("The input should be a labelled vector.\n\n", call. = FALSE)
    }
    
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



`to_labels` <- function(x) {

    if (!inherits(x, "haven_labelled")) {
        cat("\n")
        stop("The input should be a labelled vector.\n\n", call. = FALSE)
    }
    if (is.double(x)) {
        tagged <- haven::is_tagged_na(x)
    }
    else {
        tagged <- logical(length(x))
    }
    
    labels <- names_values(x)
    result <- suppressMessages(labelled::remove_labels(x))

    if (is.double(labels)) {
        taglab <- haven::is_tagged_na(labels)
        labels[taglab] <- haven::na_tag(labels[taglab])
    }

    if (any(tagged)) {
        result[tagged] <- haven::na_tag(x[tagged])
    }
    
    result[is.element(result, labels)] <- names(labels)[match(result[is.element(result, labels)], labels)]
    
    return(result)
}
