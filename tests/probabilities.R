library(statistics)

near <- function(x, y) isTRUE(all.equal(x, y))
stopifnot(near(dbinoms(c(0, 1, 1), 2, 0.5, log = TRUE), log(0.75)),
    identical(dbinoms(integer(), 2, 0.5, log = TRUE), -Inf),
    identical(dbinoms(0, 1, 1, log = TRUE), -Inf),
    near(dbinoms(1, 1, 0.25), 0.25), near(dbinoms(0, 0, 1), 1),
    near(dbinoms(0, 2000, 0.5, log = TRUE), 2000 * log(0.5)),
    inherits(try(dbinoms(1.5, 2, 0.5), silent = TRUE), "try-error"))

quiet <- capture.output(p <- dbinoms(0:1, 20, 0.5))
raw <- sum(dbinom(0:1, 20, 0.5))
log_p <- dbinoms(0:1, 20, 0.5, log = TRUE)
stopifnot(!length(quiet), identical(capture.output(p), "< 0.0001"),
    is.numeric(p), identical(as.numeric(p), raw), identical(2 * p, 2 * raw),
    identical(log(p), log(raw)), !is.object(log_p), near(log_p, log(raw)),
    identical(dbinoms(integer(), 20, 0.5), 0),
    identical(dbinoms(c(0, 14), 14, 0.5), sum(dbinom(c(0, 14), 14, 0.5))))

stopifnot(identical(as.numeric(daria("over", 9)), pnorm(9, lower.tail = FALSE)),
    identical(as.numeric(daria("between", 10, 9)), pnorm(9, lower.tail = FALSE) - pnorm(10, lower.tail = FALSE)),
    identical(as.numeric(dariat("r", 20, df = 100)), pt(20, 100, lower.tail = FALSE)),
    identical(daria("over", 9, format = TRUE), "< 0.0001"),
    identical(daria("under", -Inf, format = TRUE), "0"))

for (area in list(function(...) daria("over", 9, ...),
                  function(...) dariat("over", 20, df = 100, ...))) {
    quiet <- capture.output(p <- area())
    raw <- area(format = FALSE)
    stopifnot(!length(quiet), identical(capture.output(area()), "< 0.0001"),
        identical(capture.output(print(p)), "< 0.0001"),
        is.numeric(p), typeof(p) == "double", identical(as.numeric(p), raw),
        identical(log(p), log(raw)), identical(2 * p, 2 * raw),
        identical(p + p, raw + raw), identical(-p, -raw), identical(p < 0.0001, TRUE),
        identical(area(format = TRUE), "< 0.0001"))
}
stopifnot(identical(daria("over", 0), 0.5), identical(daria("over", Inf), 0),
    identical(capture.output(daria("over", Inf)), capture.output(print(0))),
    inherits(try(daria(NULL, 0), silent = TRUE), "try-error"))

freq <- cbind(1:3, 1)
stopifnot(near(mbinom(freq), 2), near(sbinom(freq), sqrt(2/3)),
    near(mbinom(cbind(1:2, 1e308)), 1.5), near(sbinom(cbind(1:2, 1e308)), 0.5))
for (w in list(c(-1, 2), c(0, 0), c(NA, 1))) {
    stopifnot(inherits(try(mbinom(cbind(1:2, w)), silent = TRUE), "try-error"),
        inherits(try(sbinom(cbind(1:2, w)), silent = TRUE), "try-error"))
}
x <- c(1, 5, 9)
y <- c(3, 4, 6, 12, 19)
stopifnot(near(dfcalc(x, y), unname(t.test(x, y)$parameter)),
    near(spooled(x, y), sqrt((2 * var(x) + 4 * var(y))/6)),
    near(dfcalc(1e100, 1e100, 10, 10), 18), near(dfcalc(1e-100, 1e-100, 10, 10), 18),
    near(dfcalc(x * 1e200, y * 1e200), dfcalc(x, y)),
    near(spooled(x * 1e-200, y * 1e-200) / 1e-200, spooled(x, y)),
    near(spooled(1, 1, 1500000000L, 1500000000L), 1),
    is.na(spooled(c(x, NA), y)), is.nan(dfcalc(0, 0, 3, 4)),
    inherits(try(dfcalc(1, 1, .5, .5), silent = TRUE), "try-error"),
    inherits(try(spooled(-3, 4, 5, 5), silent = TRUE), "try-error"))
