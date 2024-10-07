`t_testAB` <- function(
    x, y = NULL,
    alternative = c("two.sided", "less", "greater"), var.equal = FALSE,
    mu = 0, paired = FALSE, conf.level = 0.95, data = NULL
) {

    if (identical(alternative, c("two.sided", "less", "greater"))) {
        alternative <- "two.sided"
    }

    if (is.element(alternative, c("two.sided", "!=", "two.tailed"))) {
        pmatching <- 1
    }
    else if (is.element(alternative, c("less", "<", "lower"))) {
        pmatching <- 2
    }
    else if (is.element(alternative, c("greater", ">", "higher"))) {
        pmatching <- 3
    }
    else {
        admisc::stopError("Unknown alternative hypothesis specification.")
    }

    alternative <- c("two.sided", "less", "greater")[pmatching]

    if (!paired) {
        homogeneity <- c("not equal", "equal")

        if (is.null(y)) {
            homogtest <- ansari.test(x, data = data, exact = FALSE)
        }
        else {
            homogtest <- ansari.test(x, y, exact = FALSE)
        }

        p.value <- homogtest$p.value
        var.equal <- p.value > (1 - conf.level)
        cat (
            "\nThe Ansari-Bradley test for the homogeneity of variances has a value of ",
            paste("p = ", round(p.value, 4), sep = ""),
            " therefore the variances are ",
            homogeneity[var.equal + 1],
            "\n",
            sep = ""
        )
    }

    if (is.null(y)) {
        callist <- list(
            formula = x,
            alternative = alternative,
            conf.level = conf.level,
            var.equal = var.equal,
            data = data
        )

        if (paired) {
            callist$paired <- TRUE
        }

        test <- do.call("t.test", callist)

    } else {
        test <- t.test(
            x,
            y,
            alternative = alternative,
            conf.level = conf.level,
            paired = paired,
            var.equal = var.equal
        )
    }

    print(test)

    if (paired) {
        return(invisible(list("t.test" = test)))
    }
    else {
        return(
            invisible(
                list("Homogeneity of variances" = homogtest, "t.test" = test)
            )
        )
    }
}
