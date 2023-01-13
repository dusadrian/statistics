`mbinom` <-
function(x) {
    if (ncol(x) > 2) {
        admisc::stopError("The table has more than two columns.")
    }
    
    if (!is.numeric(x[, 1])) {
        admisc::stopError("The first column should be numeric.")
    }
    
    if (!is.numeric(x[, 2])) {
        admisc::stopError("The second column should be numeric.")
    }
    
    if (sum(x[, 2] != 1)) {
        px <- proportions(x[, 2])
    } else {
        px <- x[, 2]
    }
    
    return(sum(x[, 1]*px))
}
