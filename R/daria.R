`daria` <-
function (area, z1, z2, draw = FALSE) {
    
    if (missing(area) | is.numeric(area)) {
        cat("\n")
        stop("Unspecified area.\n\n", call. = FALSE)
    } else if (area == "exact" | area == "e") {
        cat("\n")
        cat("The area of an exact value, out of an infinity of other values, is\ninfinitely small, practically equal to zero.")
        return()
    } else if (!(area %in% c("r", "o", "right", "over",
                             "l", "u", "left", "under",
                             "b", "between"))) {
        cat("\n")
        stop("The specified area is incorrect.\n\n", call. = FALSE)
    }
    
    if (missing(z1)) {
        cat("\n")
        stop("The value of z was not specified.\n\n", call. = FALSE)
        } else {
            if (z1 > 0) {
                zet1 <- paste("+", as.character(round(z1, digits=3)), sep="") 
            } else {
                zet1 <- as.character(round(z1, digits=3))
            }
        }
    
    if (area %in% c("b", "between") & !missing(z1) & missing(z2)) {
        cat("\n")
        stop("The second z value was not specified.\n\n", call. = FALSE)
        }
    
    if (!missing(z2)) {
        if (z2 > 0) {
            zet2 <- paste("+", as.character(round(z2, digits=3)), sep="") 
        } else {
            zet2 <- as.character(round(z2, digits=3))
        }
    }
    
    if (draw) {
        if (dev.cur() == 1) {
            dev.new(width=30/2.54, height=21/2.54)
        }
        plot(seq(-4, 4, length=1000), seq(0, 0.4, length=1000), type="n", xlab="", ylab="", axes=FALSE)
    }
    
    if (area %in% c("r", "o", "right", "over")) {
        area <- 1 - pnorm(z1)
        if (abs(z1) > 4) {z1 <- 4*sign(z1)}
        xdreapta <- seq(z1, 4, 0.001)
        ydreapta <- dnorm(xdreapta)
        if (draw) {
            polygon(c(xdreapta, rev(xdreapta)), c(rep(0, length(xdreapta)), rev(ydreapta)), border=NA, col="#79a74c")
            segments(z1, 0, z1, dnorm(z1))
            # text(rep(-2.8, 4), c(0.3, 0.28, 0.26, 0.24),
            #     c(paste("The area to the right of", paste("z=", zet1, sep=""),
            #       "has:", sep=" "), round(area, 3), "or",
            #       paste(round(area, 3)*100,"%", sep="")),
            #       col=rep(c("black", "#cb2626"), 2))
            }
    } else if (area %in% c("u", "l", "under", "left")) {
        area <- pnorm(z1)
        if (abs(z1) > 4) {z1 <- 4*sign(z1)}
        xstanga <- seq(-4, z1, 0.001)
        ystanga <- dnorm(xstanga)
        if (draw) {
            polygon(c(xstanga, rev(xstanga)), c(rep(0, length(xstanga)), rev(ystanga)), border=NA, col="#79a74c" )
            segments(z1, 0, z1, dnorm(z1))
            # text(rep(-2.8, 4), c(0.3, 0.28, 0.26, 0.24),
            #     c(paste("The area to the left of", paste("z=", zet1, sep=""),
            #       "has:", sep=" "), round(area, 3), "or",
            #       paste(round(area, 3)*100, "%", sep="")),
            #       col=rep(c("black", "#cb2626"), 2))
            }
    } else if (area %in% c("b", "between")) {
        if (z1 > z2) {sz <- z1; z1 <- z2; z2 <- sz}
        area <- pnorm(z2) - pnorm(z1)
        if (abs(z1) > 4) {z1 <- 4*sign(z1)}
        if (abs(z2) > 4) {z2 <- 4*sign(z2)}
        xintre <- seq(z1, z2, 0.001)
        yintre <- dnorm(xintre)
        if (draw) {
            polygon( c(xintre,rev(xintre)), c(rep(0,length(xintre)),rev(yintre)), border=NA, col="#79a74c" )
            segments(z1, 0, z1, dnorm(z1))
            segments(z2, 0, z2, dnorm(z2))
            # text(rep(-2.8, 4), c(0.3, 0.28, 0.26, 0.24),
            #     c(paste("The area between", paste("z=", zet1, sep=""),
            #       "and", paste("z=", zet2, sep=""),
            #       "has:", sep=" "), round(area, 3), "or",
            #       paste(round(area, 3)*100, "%", sep="")),
            #       col=rep(c("black", "#cb2626"), 2))
        }
    }
    
    if (draw) {
        eval(parse(text = "curve(dnorm(x, mean=0, sd=1), from=-4, to=4, las=1, add=TRUE)"))
        segments(-4, 0, 4, 0)
        segments(-4:4, rep(0, 9), -4:4, rep(-0.004, 9))
        text(-4:4, rep(-0.01, 9), c(-4:0, paste("+", as.character(1:4), sep="")), cex=0.8)
    }
    return(area)
}

