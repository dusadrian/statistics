`anovaFK` <- function(x, y = NULL, data) {

    m <- match.call()

    name_values_x <- NULL
    name_group_y <- NULL

    if (is.null(y)) {
        if (missing(x) || length(x) != 3) {
            admisc::stopError("The formula is incorrect.")
        }

        name_values_x <- all.vars(substitute(x)[[2]])[1]
        name_group_y  <- all.vars(substitute(x)[[3]])[1]

        if (missing(data)) {
            values_x <- admisc::recreate(substitute(x)[[2]])
            group_y  <- admisc::recreate(substitute(x)[[3]])
            if (is.character(group_y)) {
                group_y <- as.factor(group_y)
            }
            homog_test <- fligner.test(values_x, group_y)
        }
        else {
            values_x <- data[, name_values_x]
            group_y <- data[, name_group_y]
            if (is.character(group_y)) {
                group_y <- as.factor(group_y)
            }
            homog_test <- fligner.test(values_x, group_y)
        }
    }
    else {
        if (is.character(y)) {
            y <- as.factor(y)
        }
        homog_test <- fligner.test(x, y)
        values_x <- x
        group_y <- y
    }


    if (!is.factor(group_y)) {
        group_y <- as.factor(group_y)
    }

    result <- list(
        homog_test = homog_test
    )

    if (is.null(name_group_y)) {
        result$test <- aov(values_x ~ group_y)
    }
    else {
        testdf <- na.omit(
            data.frame(values_x = values_x, group_y = group_y)
        )

        colnames(testdf) <- c(name_values_x, name_group_y)

        result$test <- eval(
            parse(
                text = paste(
                    "aov(",
                    name_values_x,
                    " ~ ",
                    name_group_y,")",
                    sep = ""
                )
            ),
            envir = testdf
        )
    }

    output_table <- as.data.frame(matrix(NA, nrow = 2, ncol = 5))
    rownames(output_table) <- c("group", "Residuals")
    colnames(output_table) <- c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
    tabel <- anova(lm(values_x ~ as.factor(group_y)))

    if (is.null(name_group_y)) {
        test <- oneway.test(values_x ~ as.factor(group_y))
    }
    else {
        testdf <- na.omit(
            data.frame(values_x = values_x, group_y = as.factor(group_y))
        )

        colnames(testdf) <- c(name_values_x, name_group_y)

        test <- eval(
            parse(
                text = paste(
                    "oneway.test(",
                    name_values_x,
                    " ~ ",
                    name_group_y,
                    ")",
                    sep = ""
                )
            ),
            envir = testdf
        )
    }

    output_table[ , 1] <- test$parameter
    output_table[ , 2] <- round(tabel$Sum, 2)
    output_table[ , 3] <- round(tabel$Mean, 2)
    output_table[1, 4] <- round(test$statistic, 4)
    output_table[1, 5] <- round(test$p.value, 4)

    output_table[1, 3] <- output_table[1, 4] * output_table[2, 3]
    output_table[1, 2] <- output_table[1, 3] * output_table[1, 1]
    output_table[2, 4] <- ""
    output_table[2, 5] <- ""

    result$output_table <- output_table

    class(result) <- c("anovaFK")
    return(result)
}

