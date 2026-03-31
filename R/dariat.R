`dariat` <-
function (area, t1, t2, df, draw = FALSE) {

    if (missing(area) || !is.character(area)) {
        admisc::stopError("Unspecified area.")
    }

    area <- tryCatch(
        match.arg(area, c("right", "over", "above", "left", "under", "between")),
        error = function(e) admisc::stopError("The specified area is incorrect.")
    )

    if (missing(t1)) {
        admisc::stopError("The value of t was not specified.")
    }

    if (identical(area, "between") & missing(t2)) {
        admisc::stopError("The second t value was not specified.")
    }

    if (missing(df)) {
        admisc::stopError("The degrees of freedom were not specified.")
    }

    limit <- 4 + ifelse(df < 21, 1, 0)

    if (is.element(area, c("right", "over", "above"))) {
        result <- 1 - pt(t1, df)
        if (abs(t1) > limit) {t1 <- limit*sign(t1)}
    } else if (is.element(area, c("under", "left"))) {
        result <- pt(t1, df)
        if (abs(t1) > limit) {
            t1 <- limit*sign(t1)
        }

    } else if (identical(area, "between")) {
        if (t1 > t2) {temp <- t1; t1 <- t2; t2 <- temp}
        result <- pt(t2, df) - pt(t1, df)
        if (abs(t1) > limit) {t1 <- limit*sign(t1)}
        if (abs(t2) > limit) {t2 <- limit*sign(t2)}

    }

    if (draw) {
        old_par <- par(no.readonly = TRUE)
        on.exit(par(old_par), add = TRUE)

        if (dev.cur() == 1) {
            dev.new(width = 30/2.54, height = 21/2.54)
        }

        par(mar = c(1, 0, 0, 0))

        x <- seq(-4, 4, length.out = 1000)
        y <- dt(x, df)

        plot(
            x,
            y,
            type = "l",
            xaxt = "n",
            yaxt = "n",
            ylab = "",
            xlab = "",
            ylim = c(0, max(y)),
            bty = "n"
        )

        if (is.element(area, c("right", "over", "above"))) {
            xright <- seq(t1, limit, 0.001)
            yright <- dt(xright, df)
            polygon(
                c(xright, rev(xright)),
                c(rep(0, length(xright)), rev(yright)),
                border = NA,
                col = "#79a74c"
            )
            segments(t1, 0, t1, dt(t1, df))
        } else if (is.element(area, c("under", "left"))) {
            xleft <- seq(-limit, t1, 0.001)
            yleft <- dt(xleft, df)
            polygon(
                c(xleft, rev(xleft)),
                c(rep(0, length(xleft)), rev(yleft)),
                border = NA,
                col = "#79a74c"
            )
            segments(t1, 0, t1, dt(t1, df))
        } else if (identical(area, "between")) {
            xintre <- seq(t1, t2, 0.001)
            yintre <- dt(xintre, df)
            polygon(
                c(xintre, rev(xintre)),
                c(rep(0, length(xintre)), rev(yintre)),
                border = NA,
                col = "#79a74c"
            )
            segments(c(t1, t2), 0, c(t1, t2), dt(c(t1, t2), df))
        }

        RStudio <- identical(names(dev.cur()), "RStudioGD")
        Positron <- grepl("Positron", names(dev.cur()))

        tick_length <- ifelse(Positron|RStudio, 0.02, 0.015)
        label_offset <- ifelse (Positron|RStudio, -1, -1.2)


        ticklabels <- c(-4:0, paste("+", as.character(1:4), sep = ""))

        axis(
            side = 1,
            at = seq(-4, 4, by = 1),
            labels = TRUE,
            cex.axis = ifelse(Positron|RStudio, 0.8, 1),
            lwd = 0,
            line = label_offset
        )

        segments(-4, 0, 4, 0)
        segments(-4:4, y0 = 0, x1 = -4:4, y1 = -tick_length * (par("usr")[4] - par("usr")[3]))
    }

    return(result)
}
