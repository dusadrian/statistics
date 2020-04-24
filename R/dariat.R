`dariat` <-
function (area, t1, t2, df, draw=FALSE) {
    
    if (missing(area) | is.numeric(area)) {
        cat("\n")
        stop("Unspecified area.\n\n", call. = FALSE)
    } else if (area == "exact" | area == "e") {
        cat("\n")
        cat("The area of an exact value, out of an infinity of other\nvalues,",
            "is infinitely small, practically equal to zero.")
        return()
    } else if (!(area %in% c("r", "o", "right", "over",
                             "l", "u", "left", "under",
                             "b", "between"))) {
        cat("\n")
        stop("The specified area is incorrect.\n\n", call. = FALSE)
    }
    
    if (missing(t1)) {
        cat("\n")
        stop("The value of t was not specified.\n\n", call. = FALSE)
        } else {
            if (t1 > 0) {
                te1 <- paste("+", as.character(round(t1, digits=3)), sep="") 
            } else {
                te1 <- as.character(round(t1, digits=3))
            }
        }
    
    if (area %in% c("b", "between") & !missing(t2) & missing(t2)) {
        cat("\n")
        stop("The second t value was not specified.\n\n", call. = FALSE)
        }
    
    if (!missing(t2)) {
        if (t2 > 0) {
            te2 <- paste("+", as.character(round(t2, digits=3)), sep="") 
        } else {
            te2 <- as.character(round(t2, digits=3))
        }
    }
    
    if (missing(df)) {
        cat("\n")
        stop("The degrees of freedom were not specified.\n\n", call. = FALSE)
    }
    
    limita <- 4 + ifelse(df < 21, 1, 0)
    
    if (draw) {
        if (dev.cur() == 1) {
            dev.new(width=30/2.54, height=21/2.54)
        }
        plot(seq(-limita, limita, length=1000),
             seq(0, 0.4, length=1000),
             type="n", xlab="", ylab="", axes=FALSE)
    }
    
    if (area %in% c("r", "o", "right", "over")) {
        area <- 1 - pt(t1, df)
        if (abs(t1) > limita) {t1 <- limita*sign(t1)}
        xdreapta <- seq(t1, limita, 0.001)
        ydreapta <- dt(xdreapta, df)
        if (draw) {
            polygon(c(xdreapta, rev(xdreapta)),
                    c(rep(0, length(xdreapta)), rev(ydreapta)),
                    border=NA, col="#79a74c")
            segments(t1, 0, t1, dt(t1, df))
            text(rep(-3.2 + 0.5*(limita == 4), 5), seq(0.32, 0.24, -0.02),
                c(paste("At", df, "degrees of freedom,"), 
                  paste("the area to the right of", paste("t=", te1, sep=""),
                  "has:", sep=" "), round(area, 3), "or", 
                  paste(round(area, 3)*100,"%", sep="")),
                  col=c("black", rep(c("black", "#cb2626"), 2)))
            command <- paste("curve(dt(x, df), from=-", limita, ", to=", limita, ", las=1, add=TRUE)", sep = "")
            eval(parse(text = command))
        }
    } else if (area %in% c("l", "u", "left", "under")) {
        area <- pt(t1, df)
        if (abs(t1) > limita) {t1 <- limita*sign(t1)}
        xstanga <- seq(-limita, t1, 0.001)
        ystanga <- dt(xstanga, df)
        if (draw) {
            polygon(c(xstanga, rev(xstanga)),
                    c(rep(0, length(xstanga)), rev(ystanga)),
                    border=NA, col="#79a74c" )
            segments(t1, 0, t1, dt(t1, df))
            text(rep(-3.2 + 0.5*(limita == 4), 5), seq(0.32, 0.24, -0.02),
                c(paste("At", df, "degrees of freedom,"), 
                  paste("the area to the left of", paste("t=", te1, sep=""),
                  "has:", sep=" "), round(area, 3), "or", 
                  paste(round(area, 3)*100, "%", sep="")),
                  col=c("black", rep(c("black", "#cb2626"), 2)))
            command <- paste("curve(dt(x, df), from=-", limita, ", to=", limita, ", las=1, add=TRUE)", sep = "")
            eval(parse(text = command))
            # curve(dt(x, df), from=-limita, to=limita, las=1, add=TRUE)
        }
    } else if (area %in% c("b", "between")) {
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
            text(rep(-3.2 + 0.5*(limita == 4), 5), seq(0.32, 0.24, -0.02),
                c(paste("At", df, "degrees of freedom,"), 
                  paste("the area between", paste("t=", te1, sep=""),
                  "and", paste("t=", te2, sep=""),
                  "has:", sep=" "), round(area, 3), "or",
                  paste(round(area, 3)*100, "%", sep="")),
                  col=c("black", rep(c("black", "#cb2626"), 2)))
            command <- paste("curve(dt(x, df), from=-", limita, ", to=", limita, ", las=1, add=TRUE)", sep = "")
            eval(parse(text = command))
        }
    }
    
    if (draw) {
        segments(-limita, 0, limita, 0)
        segments(-limita:limita, rep(0, limita*2 + 1),
                 -limita:limita, rep(-0.004, limita*2 + 1))
        text(-limita:limita, rep(-0.01, limita*2 + 1),
             c(-limita:0, paste("+", as.character(1:limita), sep="")),
             cex=0.8)
    }
    return(area)
}

