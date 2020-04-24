`fmode` <- function(x) {
    tbl <- table(x)
    fmode <- names(tbl[which.max(tbl)])
    if (possibleNumeric_statistics(fmode)) {
        fmode <- asNumeric_statistics(fmode)
    }
    return(fmode)
}



`mod` <- function(...) {
    fmode(...)
}
