library(statistics)

d <- data.frame(value = c(1, 3, 4, 2, 5, 9, 7, 8, 10),
    group = factor(rep(LETTERS[1:3], each = 3)))
for (pooled in c(FALSE, TRUE)) {
    for (alternative in c("two.sided", "less", "greater")) {
        fit <- t.testhv(value ~ group, d, subset = group != "C", var.equal = pooled,
            alternative = alternative)
        ref <- t.test(value ~ group, d, subset = group != "C", var.equal = pooled,
            alternative = alternative)
        stopifnot(identical(fit$selected$p.value, ref$p.value),
            identical(fit$selected$conf.int, ref$conf.int),
            identical(fit$selected$estimate, ref$estimate))
    }
}

x <- c(1, 4, NA, 7, 9, 11)
y <- c(2, 3, 8, NA, 6, 13)
paired_fits <- list(t.testhv(x, y, paired = TRUE))
if (getRversion() >= "4.0.0") {
    paired_fits <- c(paired_fits, list(t.testhv(Pair(x, y)), t.testhv(Pair(x, y) ~ 1)))
}
for (fit in paired_fits) {
    stopifnot(identical(fit$selected$p.value, t.test(x, y, paired = TRUE)$p.value),
        identical(fit$ttest, fit$ttestWelch))
}
one <- t.testhv(x ~ 1)
stopifnot(identical(one$selected$p.value, t.test(x)$p.value))
single <- t.testhv(3, 4:8, var.equal = TRUE)
stopifnot(identical(single$selected$p.value, t.test(3, 4:8, var.equal = TRUE)$p.value),
    is.null(single$ttestWelch))
grp <- declared::declared(c(1, 2), labels = c(A = 1, B = 2), measurement = "categorical")
for (bad in list(quote(t.testhv(x, paired = TRUE)), quote(t.testhv(x, grp)),
    quote(t.testhv(x, mu0 = 0)), quote(t.testhv(x, var.equal = NA)),
    quote(t.testhv(value ~ group, d, paired = TRUE)))) {
    stopifnot(inherits(try(eval(bad), silent = TRUE), "try-error"))
}
stopifnot(identical(t.testhv(d$value[1:6], d$group[1:6])$selected$p.value,
    t.testhv(value ~ group, d, subset = group != "C")$selected$p.value))
fit <- t.testAB(100:120)
display <- capture.output(returned <- print(fit))
stopifnot(any(grepl("p-value < 0.0001", display, fixed = TRUE)), identical(returned, fit))

# Labels on a numeric response must survive as sample values, including with
# different group sizes and a declared missing value.
ratings <- data.frame(score = declared::declared(c(1, 2, 3, 4, 2, 3, 1, -99),
    labels = c(low = 1, fair = 2, good = 3, high = 4), na_values = -99,
    measurement = "ordinal"), group = rep(c("A", "B"), c(3, 5)))
reference <- t.test(as.numeric(score) ~ group, ratings)
for (fit in list(t.testhv(score ~ group, ratings, var.equal = FALSE),
    t.testhv(ratings$score, ratings$group, var.equal = FALSE))) {
    stopifnot(identical(fit$selected$p.value, reference$p.value),
        identical(fit$selected$estimate, reference$estimate))
}
stopifnot(inherits(try(t.testhv(x ~ 1, y = y), silent = TRUE), "try-error"),
    inherits(try(t.testhv(1, 2:4, var.equal = c(setting = FALSE)), silent = TRUE), "try-error"),
    identical(t.testhv(x ~ 1, alternative = NULL, mu = NULL)$selected$p.value,
        t.test(x)$p.value))
