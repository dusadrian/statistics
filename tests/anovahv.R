library(statistics)


analysis_data <- data.frame(
    values = declared::declared(
        c(2, 4, -99, 6, 8, 10, 3, 5, NA, 7, 9, 11),
        na_values = -99
    ),
    groups = rep(c("A", "B"), each = 6)
)

complete_data <- analysis_data[!is.na(analysis_data$values), ]

formula_result <- anovahv(values ~ groups, data = analysis_data)
complete_result <- anovahv(values ~ groups, data = complete_data)
default_result <- anovahv(
    analysis_data$values,
    analysis_data$groups
)

stopifnot(
    inherits(formula_result, "aov"),
    inherits(formula_result, "lm"),
    identical(class(formula_result), c("anovahv", "aov", "lm")),
    identical(formula_result$output_table, complete_result$output_table),
    identical(
        formula_result$homog_test$p.value,
        complete_result$homog_test$p.value
    ),
    identical(
        unname(formula_result$welch$p.value),
        unname(default_result$welch$p.value)
    )
)


stopifnot(
    identical(TukeyHSD(formula_result), TukeyHSD(formula_result$test)),
    identical(anova(formula_result), anova(formula_result$test)),
    identical(coef(formula_result), coef(formula_result$test)),
    identical(residuals(formula_result), residuals(formula_result$test)),
    identical(fitted(formula_result), fitted(formula_result$test)),
    identical(model.frame(formula_result), model.frame(formula_result$test)),
    identical(predict(formula_result), predict(formula_result$test)),
    inherits(summary(formula_result), "summary.aov")
)


qualified_data <- data.frame(
    values = declared::declared(
        replace(seq_len(20), 3, -99),
        na_values = -99
    ),
    groups = declared::declared(
        c(rep(seq_len(3), each = 5), rep(4, 4), 9),
        labels = c(A = 1, B = 2, C = 3, D = 4),
        na_values = 9,
        measurement = "categorical"
    )
)

data_formula_result <- anovahv(
    values ~ groups,
    data = qualified_data,
    var.equal = TRUE
)
qualified_formula_result <- anovahv(
    qualified_data$values ~ qualified_data$groups,
    var.equal = TRUE
)

stopifnot(
    isTRUE(
        all.equal(
            unname(as.matrix(anova(data_formula_result$test))),
            unname(as.matrix(anova(qualified_formula_result$test)))
        )
    ),
    nrow(model.frame(qualified_formula_result$test)) == 18L,
    df.residual(qualified_formula_result$test) == 14L,
    length(qualified_formula_result$test$na.action) == 0L
)
