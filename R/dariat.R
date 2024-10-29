`dariat` <-
function (area, t1, t2, df, draw = FALSE) {

    if (missing(area) | is.numeric(area)) {
        admisc::stopError("Unspecified area.")
    } else if (area == "exact" | area == "e") {
        cat("\n")
        cat("The area of an exact value, out of an infinity of other\nvalues,",
            "is infinitely small, practically equal to zero.")
        return()
    } else if (
        !is.element(
            area,
            c("r", "o", "a", "right", "over", "above", "l", "u", "left", "under", "b", "between")
        )
    ) {
        admisc::stopError("The specified area is incorrect.")
    }

    if (missing(t1)) {
        admisc::stopError("The value of t was not specified.")
        } else {
            if (t1 > 0) {
                te1 <- paste("+", as.character(round(t1, digits = 3)), sep = "")
            } else {
                te1 <- as.character(round(t1, digits = 3))
            }
        }

    if (area %in% c("b", "between") & !missing(t2) & missing(t2)) {
        admisc::stopError("The second t value was not specified.")
        }

    if (!missing(t2)) {
        if (t2 > 0) {
            te2 <- paste("+", as.character(round(t2, digits = 3)), sep = "")
        } else {
            te2 <- as.character(round(t2, digits = 3))
        }
    }

    if (missing(df)) {
        admisc::stopError("The degrees of freedom were not specified.")
    }

    limita <- 4 + ifelse(df < 21, 1, 0)

    if (draw) {
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
    }

    if (is.element(area, c("r", "o", "a", "right", "over", "above"))) {
        area <- 1 - pt(t1, df)
        if (abs(t1) > limita) {t1 <- limita*sign(t1)}
        xdreapta <- seq(t1, limita, 0.001)
        ydreapta <- dt(xdreapta, df)
        if (draw) {
            polygon(c(xdreapta, rev(xdreapta)),
                    c(rep(0, length(xdreapta)), rev(ydreapta)),
                    border=NA, col="#79a74c")
            segments(t1, 0, t1, dt(t1, df))
        }
    } else if (is.element(area, c("u", "l", "under", "left"))) {
        area <- pt(t1, df)
        if (abs(t1) > limita) {t1 <- limita*sign(t1)}
        xstanga <- seq(-limita, t1, 0.001)
        ystanga <- dt(xstanga, df)
        if (draw) {
            polygon(c(xstanga, rev(xstanga)),
                    c(rep(0, length(xstanga)), rev(ystanga)),
                    border=NA, col="#79a74c" )
            segments(t1, 0, t1, dt(t1, df))
        }
    } else if (is.element(area, c("b", "between"))) {
        if (t1 > t2) {temp <- t1; t1 <- t2; t2 <- temp}
        area <- pt(t2, df) - pt(t1, df)
        if (abs(t1) > limita) {t1 <- limita*sign(t1)}
        if (abs(t2) > limita) {t2 <- limita*sign(t2)}
        xintre <- seq(t1, t2, 0.001)
        yintre <- dt(xintre, df)
        if (draw) {
            polygon(c(xintre, rev(xintre)),
                    c(rep(0, length(xintre)), rev(yintre)),
                    border=NA, col="#79a74c" )
            segments(c(t1, t2), 0, c(t1, t2), dt(c(t1, t2), df))
        }
    }

    if (draw) {

        RStudio <- identical(names(dev.cur()), "RStudioGD")
        Positron <- grepl("Positron", names(dev.cur()))

        tick_length <- ifelse(Positron|RStudio, 0.02, 0.015)
        label_offset <- ifelse (Positron|RStudio, 0.1, -1.2)


        ticklabels <- c(-4:0, paste("+", as.character(1:4), sep = ""))

        axis(
            side = 1,
            at = seq(-4, 4, by = 1),
            labels = TRUE,
            cex.axis = ifelse(Positron|RStudio, 0.8, 1),
            lwd = 0,
            line = label_offset
        )

        segments(-4:4, y0 = 0, x1 = -4:4, y1 = -tick_length * (par("usr")[4] - par("usr")[3]))
        segments(-4, 0, 4, 0)
    }
    return(area)
}
