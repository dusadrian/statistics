`spooled` <- 
function (x, y, n1, n2) {
    
    erori <- c("The first variable was not specified",
               "The second variable was not specified",
               "The standard deviation for the first group (sd1) was not specified",
               "The standard deviation for the second group (sd2) was not specified",
               "The sample size for the first group (n1) was not specified",
               "The sample size for the second group (n2) was not specified",
               "The first argument should be a group with multiple values",
               "The second argument should be a group with multiple values")
    
    verificari <- c(missing(x), missing(y), missing(n1), missing(n2))
    
    if (sum(verificari) == 4) {
        if (sum(verificari[1:2]) == 0) {
            admisc::stopError(
                paste(
                    erori[3:6][verificari[1:4]],
                    collapse = "\n       "
                )
            )
        }
        else {
            admisc::stopError(
                paste(
                    erori[1:2][verificari[1:2]],
                    collapse = "\n       "
                )
            )
        }
    }
    else if (sum(verificari) == 3) {
        if (length(x) == 1) {
            admisc::stopError(
                paste(
                    c(
                        paste(
                            "If",
                            x,
                            "is the standard deviation for the first group, then:"
                        ),
                        erori[4:6]
                    ),
                    collapse = "\n       "
                )
            )
        }
        else {
            admisc::stopError(erori[2])
        }
    }
    else if (sum(verificari) == 2) {
        if (missing(x) | missing(y)) {
            admisc::stopError(
                paste(
                    erori[1:2][verificari[1:2]],
                    collapse = "\n       "
                )
            )
        }
        else if (all(length(x) == 1, length(y) == 1)) {
            admisc::stopError(
                paste(erori[5:6], collapse = "\n       ")
            )
        }
        else if (any(length(x) == 1, length(y) == 1)) {
            admisc::stopError(
                erori[7:8][which(c(length(x) == 1, length(y) == 1))]
            )
        }
        else {
            var1 <- var(x)
            n1 <- length(x)
            var2 <- var(y)
            n2 <- length(y)
        }
    }
    else if (sum(verificari) == 1) {
        admisc::stopError(
            paste(
                erori[3:6][verificari[1:4]],
                collapse = "\n       "
            )
        )
    }
    else {
        var1 <- x^2
        var2 <- y^2
    }
    
    return(sqrt(((n1 - 1)*var1 + (n2 - 1)*var2)/(n1 + n2 - 2)))
}
