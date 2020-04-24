`trimstr_statistics` <-
function(x, what = " ", side = "both") {
    what <- ifelse(what == " ", "[[:space:]]", ifelse(what == "*", "\\*", what))
    pattern <- switch(side,
    both = paste("^", what, "+|", what, "+$", sep = ""),
    left = paste("^", what, "+", sep = ""),
    right = paste(what, "+$", sep = "")
    )
    gsub(pattern, "", x)
}


`splitstr_statistics` <-
function(x) {
    
    if (identical(x, "")) return(x)
    
    y <- gsub("\\n", "", unlist(strsplit(gsub("[[:space:]]", "", x), split = ",")))
    
    if (length(y) == 1) {
        # try again, using a semicolon
        y <- gsub("\\n", "", unlist(strsplit(gsub("[[:space:]]", "", y), split = ";")))
    }
    
    metacall <- match.call()$x
    
    if (metacall == "sort.by") {
        if (any(grepl("[=]", y))) {
            y <- t(as.data.frame(strsplit(y, split = "=")))
            values <- y[, 2] == TRUE
            names(values) <- y[, 1]
        }
        else {
            values <- !grepl("[+]", y)
            names(values) <- gsub("[+|-]", "", y)
        }
        return(values)
    }
    else if (metacall == "decreasing") {
        return(as.logical(y))
    }
    else if (metacall == "thresholds") {
        if (any(grepl("[=]", y))) {
            y <- t(as.data.frame(strsplit(y, split = "=")))
            values <- y[, 2]
            if (possibleNumeric_statistics(values)) {
                values <- asNumeric_statistics(values)
            }
            names(values) <- y[, 1]
        }
        else {
            if (possibleNumeric_statistics(y)) {
                values <- asNumeric_statistics(y)
            }
        }
        return(values)
    }
    else {
        if (possibleNumeric_statistics(y)) {
            y <- asNumeric_statistics(y)
        }
        return(y)
    }
    
}



`getName_statistics` <-
function(x) {
    result <- rep("", length(x))
    x <- as.vector(gsub("1-", "", gsub("[[:space:]]", "", x)))
    
    for (i in seq(length(x))) {
                                        
        condsplit <- unlist(strsplit(x[i], split=""))
        
        startpos <- 0
        keycode <- ""
        
        if (any(condsplit == "]")) {
            startpos <- max(which(condsplit == "]"))
            keycode <- "]"
        }
        
        
        if (any(condsplit == "$")) {
            sp <- max(which(condsplit == "$"))
            if (sp > startpos) {
                startpos <- sp
                keycode <- "$"
            }
        }
        
        
        # if (identical(keycode, "")) {
        #     result <- toupper(x)
        # }
        # else
        if (identical(keycode, "$")) {
            # result <- toupper(substring(x, startpos + 1))
            result[i] <- substring(x[i], startpos + 1)
        }
        else if (identical(keycode, "]")) {
            
            # keycode is "]"
            # this is a matrix or a list
            # determine where the indexing starts
            stindex <- max(which(condsplit == "["))
            
            filename <- paste(condsplit[seq(ifelse(any(condsplit == "("), which(condsplit == "("), 0) + 1, which(condsplit == "[") - 1)], collapse="")
            
            # ptn = possibly the name
            ptn <- substr(x, stindex + 1, startpos)
            postring <- grepl("\"", ptn)
            ptn <- gsub("\"|]|,|\ ", "", ptn)
            
            # ptn <- unlist(strsplit(ptn, split=":"))
            stopindex <- ifelse(identical(condsplit[stindex - 1], "["), stindex - 2, stindex - 1)
            
            # determine if what remains is a number or a name
            if (possibleNumeric_statistics(ptn)) {
                # it's a number (an index)
                # see if it has column names
                
                # stopindex <- ifelse(identical(condsplit[stindex - 1], "["), stindex - 2, stindex - 1)
                cols <- eval.parent(parse(text=paste("colnames(", filename, ")", sep="")))
                
                if (!is.null(cols)) {
                    # result <- toupper(cols[as.numeric(ptn)])
                    result[i] <- cols[as.numeric(ptn)]
                }
            }
            else {
                # it's a name
                # result <- toupper(ptn)
                
                # just to make sure it's not something like "1:2"
                if (!grepl(":", ptn)) {
                    result <- ptn
                }
                
                if (!postring) { # could be something like mydf[, i] and ptn = i here
                    ptnfound <- FALSE
                    n <- 1
                    
                    if (eval.parent(parse(text=paste0("\"", ptn, "\" %in% ls()")), n = 1)) {
                        ptn <- eval.parent(parse(text=paste("get(", ptn, ")", sep="")), n = 1)
                        ptnfound <- TRUE
                    }
                    else if (eval.parent(parse(text=paste0("\"", ptn, "\" %in% ls()")), n = 2)) {
                        ptn <- eval.parent(parse(text=paste("get(\"", ptn, "\")", sep="")), n = 2)
                        ptnfound <- TRUE
                        n <- 2
                    }
                    
                    if (ptnfound) {
                        # check if it's a number
                        if (possibleNumeric_statistics(ptn)) {
                            result <- eval.parent(parse(text=paste("colnames(", filename, ")[", ptn, "]", sep="")), n = n)
                        }
                        else {
                            result <- ptn
                        }
                    }
                }
            }
        }
        else {
            result <- x
        }
    }
    return(gsub(",|\ ", "", result))
}


`possibleNumeric_statistics` <-
function(x) {
    # as.character converts everything (especially factors)
    return(!any(is.na(suppressWarnings(as.numeric(na.omit(as.character(x)))))) & !all(is.na(x)))
}


`asNumeric_statistics` <-
function(x) {
    return(suppressWarnings(as.numeric(as.character(x))))
}
