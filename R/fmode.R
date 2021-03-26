`fmode` <- function(x) {
    tbl <- table(x)
    fmode <- names(tbl[which.max(tbl)])
    if (admisc::possibleNumeric(fmode)) {
        fmode <- admisc::asNumeric(fmode)
    }
    return(fmode)
}



`mod` <- function(...) {
    .Deprecated(msg = "Function mod() is deprecated, and has been renamed to fmode()\n")
    fmode(...)
}
