`daria` <-
function (area, z1, z2, draw = FALSE) {

    if (missing(area) | is.numeric(area)) {
        cat("\n")
        admisc::stopError("Unspecified area.")
    } else if (area == "exact" | area == "e") {
        cat("\n")
        cat("The area of an exact value, out of an infinity of other values, is\ninfinitely small, practically equal to zero.")
        return()
    } else if (
        !is.element(
            area,
            c("r", "o", "a", "right", "over", "above", "l", "u", "left", "under", "b", "between")
        )
    ) {
        admisc::stopError("The specified area is incorrect.")
    }

    if (missing(z1)) {
        admisc::stopError("The value of z was not specified.")
        } else {
            if (z1 > 0) {
                zet1 <- paste("+", as.character(round(z1, digits = 3)), sep = "")
            } else {
                zet1 <- as.character(round(z1, digits = 3))
            }
        }

    if (is.element(area, c("b", "between")) & !missing(z1) & missing(z2)) {
        admisc::stopError("The second z value was not specified.")
        }

    if (!missing(z2)) {
        if (z2 > 0) {
            zet2 <- paste("+", as.character(round(z2, digits = 3)), sep = "")
        } else {
            zet2 <- as.character(round(z2, digits = 3))
        }
    }

    if (draw) {
        if (dev.cur() == 1) {
            dev.new(width = 30/2.54, height = 21/2.54)
        }

        par(mar = c(1, 0, 0, 0))

        x <- seq(-4, 4, length.out = 1000)
        y <- dnorm(x)

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
        area <- 1 - pnorm(z1)
        if (abs(z1) > 4) {z1 <- 4*sign(z1)}
        xdreapta <- seq(z1, 4, 0.001)
        ydreapta <- dnorm(xdreapta)
        if (draw) {
            polygon(
                c(xdreapta, rev(xdreapta)),
                c(rep(0, length(xdreapta)), rev(ydreapta)),
                border = NA,
                col = "#79a74c"
            )
            segments(z1, 0, z1, dnorm(z1))
            }
    } else if (is.element(area, c("u", "l", "under", "left"))) {
        area <- pnorm(z1)
        if (abs(z1) > 4) {
            z1 <- 4*sign(z1)
        }

        xstanga <- seq(-4, z1, 0.001)
        ystanga <- dnorm(xstanga)

        if (draw) {
            polygon(
                c(xstanga, rev(xstanga)),
                c(rep(0, length(xstanga)), rev(ystanga)),
                border = NA,
                col = "#79a74c"
            )

            segments(z1, 0, z1, dnorm(z1))
        }
    } else if (is.element(area, c("b", "between"))) {
        if (z1 > z2) {
            sz <- z1; z1 <- z2; z2 <- sz
        }

        area <- pnorm(z2) - pnorm(z1)
        if (abs(z1) > 4) {
            z1 <- 4*sign(z1)
        }

        if (abs(z2) > 4) {
            z2 <- 4*sign(z2)
        }

        xintre <- seq(z1, z2, 0.001)
        yintre <- dnorm(xintre)
        if (draw) {
            polygon(
                c(xintre, rev(xintre)),
                c(rep(0,length(xintre)), rev(yintre)),
                border = NA,
                col = "#79a74c"
            )

            segments(z1, 0, z1, dnorm(z1))
            segments(z2, 0, z2, dnorm(z2))
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
