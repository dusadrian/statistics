`frmode` <- function(x) {
    tbl <- table(x)
    fmode <- names(tbl[which.max(tbl)])
    if (admisc::possibleNumeric(fmode)) {
        fmode <- admisc::asNumeric(fmode)
    }
    return(fmode)
}



`fmode` <- function(...) {
    .Deprecated(msg = "Function fmode() is deprecated, and has been renamed to frmode()\n")
    fmode(...)
}
