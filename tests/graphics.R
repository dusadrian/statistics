library(statistics)

# A calculation-only histogram must leave the active device alone.
device <- dev.cur()
h <- histc(0:9, breaks = c(0, 4.5, 9), plot = FALSE)
stopifnot(identical(dev.cur(), device), identical(h$counts, c(5L, 5L)),
    sum(histc(0:9, from = 0, to = 9, plot = FALSE)$counts) == 10L)
breaks <- c(0, 1, 5, 10)
stopifnot(identical(histc(1:10, , , , breaks, plot = FALSE)$breaks, breaks),
    identical(histc(1:10, brea = breaks, plot = c(draw = FALSE))$breaks, breaks),
    identical(dev.cur(), device))

directory <- tempfile()
dir.create(directory)
pdf(file.path(directory, "panel-%d.pdf"), onefile = FALSE)
par(mfrow = c(2, 2))
margins <- par("mar")
for (freq in c(FALSE, TRUE)) {
    h <- histc(0:9, breaks = c(0, 4.5, 9), freq = freq, yaxs = "i")
    peak <- dnorm(4.5, 4.5, sd(0:9)) * if (freq) 45 else 1
    stopifnot(isTRUE(all.equal(par("usr")[4L], 1.1 * peak)), identical(par("mar"), margins))
}
daria("over", 9, draw = TRUE)
dariat("between", -5, 5, df = 5, draw = TRUE)
dev.off()
stopifnot(length(list.files(directory)) == 1L)

pdf(file.path(directory, "binomial-%d.pdf"), onefile = FALSE)
p <- dbinoms(0:1, 2, 0.5, log = TRUE, draw = TRUE, zoom = TRUE, text = TRUE)
dev.off()
stopifnot(length(list.files(directory, pattern = "binomial")) == 1L,
    isTRUE(all.equal(p, log(0.75))))
unlink(directory, recursive = TRUE)
