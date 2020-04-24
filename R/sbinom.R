`sbinom` <-
function(x) {
    if (ncol(x) > 2) {
        cat("\n")
        stop("The table has more than two columns.\n\n", call. = FALSE)
    }
    
    if (!is.numeric(x[, 1])) {
        cat("\n")
        stop("The first column should be numeric.\n\n", call. = FALSE)
    }
    
    if (!is.numeric(x[, 2])) {
        cat("\n")
        stop("The second column should be numeric.\n\n", call. = FALSE)
    }
    
    if (sum(x[, 2] != 1)) {
        px <- prop.table(x[, 2])
    } else {
        px <- x[, 2]
    }
    
    return(sqrt(sum((x[, 1] - sum(x[, 1]*px))^2*px)))
}

