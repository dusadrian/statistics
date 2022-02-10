`frmode` <- function(x) {
    tbl <- table(x)
    wm <- which.max(tbl)
    
    fmode <- names(tbl[wm])
    if (admisc::possibleNumeric(fmode)) {
        fmode <- admisc::asNumeric(fmode)
    }

    if (length(tbl[tbl == wm]) > 1) {
        message("Multiple modes detected, only the first is returned.")
    }
    return(fmode)
}
