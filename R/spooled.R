`spooled` <-
function (x, y, n1, n2) {

    errors <- c(
        "The first variable was not specified",
        "The second variable was not specified",
        "The standard deviation for the first group (sd1) was not specified",
        "The standard deviation for the second group (sd2) was not specified",
        "The sample size for the first group (n1) was not specified",
        "The sample size for the second group (n2) was not specified",
        "The first argument should be a group with multiple values",
        "The second argument should be a group with multiple values"
    )

    checks <- c(missing(x), missing(y), missing(n1), missing(n2))

    if (sum(checks) == 4) {
        if (sum(checks[1:2]) == 0) {
            admisc::stopError(
                paste(
                    errors[3:6][checks[1:4]],
                    collapse = "\n       "
                )
            )
        }
        else {
            admisc::stopError(
                paste(
                    errors[1:2][checks[1:2]],
                    collapse = "\n       "
                )
            )
        }
    }
    else if (sum(checks) == 3) {
        if (length(x) == 1) {
            admisc::stopError(
                paste(
                    c(
                        paste(
                            "If",
                            x,
                            "is the standard deviation for the first group, then:"
                        ),
                        errors[4:6]
                    ),
                    collapse = "\n       "
                )
            )
        }
        else {
            admisc::stopError(errors[2])
        }
    }
    else if (sum(checks) == 2) {
        if (missing(x) | missing(y)) {
            admisc::stopError(
                paste(
                    errors[1:2][checks[1:2]],
                    collapse = "\n       "
                )
            )
        }
        else if (all(length(x) == 1, length(y) == 1)) {
            admisc::stopError(
                paste(errors[5:6], collapse = "\n       ")
            )
        }
        else if (any(length(x) == 1, length(y) == 1)) {
            admisc::stopError(
                errors[7:8][which(c(length(x) == 1, length(y) == 1))]
            )
        }
        else {
            var1 <- var(x)
            n1 <- length(x)
            var2 <- var(y)
            n2 <- length(y)
        }
    }
    else if (sum(checks) == 1) {
        admisc::stopError(
            paste(
                errors[3:6][checks[1:4]],
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
