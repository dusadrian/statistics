`anovaFK` <-
function(x, y = NULL, data) {
    
    m <- match.call()
    
    if (is.null(y)) {
        if (missing(x) || length(x) != 3) {
            stop("The formula is incorrect.\n\n", call. = FALSE)
            }
        values.x <- deparse(substitute(x)[[2]])
        group.y  <- deparse(substitute(x)[[3]])
        
        if (missing(data)) {
            values.x <- get(values.x)
            group.y  <- get(deparse(substitute(x)[[3]]))
            if (is.character(group.y)) {
                group.y <- as.factor(group.y)
            }
            omog.test <- fligner.test(values.x, group.y)
        }
        else {
            # omog.test <- fligner.test(x, data=data)
            values.x <- data[, values.x]
            group.y <- data[, group.y]
            if (is.character(group.y)) {
                group.y <- as.factor(group.y)
            }
            omog.test <- fligner.test(values.x, group.y)
        }
    }
    else {                     
        if (is.character(y)) {
            y <- as.factor(y)
        }
        omog.test <- fligner.test(x, y)
        values.x <- x
        group.y <- y
    }
    
    p.value <- omog.test$p.value
    # group.y <- as.factor(group.y)
    
    omogenitate <- c("do not have equal variation (using Welch approximation).", "have equal variation.")
    
    var.equal <- ifelse(p.value > 0.05, TRUE, FALSE)
    cat ("\nThe Fligner-Killeen test for the homogeneity of variances has a value of\n",
         paste("p = ", round(p.value, 4), sep=""), " so the groups ",
         omogenitate[var.equal + 1], "\n\n", sep="")
    
    if (var.equal) {
        cat("ANOVA table\n\n")
        group <- group.y
        test <- aov(values.x ~ group)
        print(summary(test))
        cat("\n")
        return(invisible(list("Homogeneity of variances" = omog.test, "aov" = test)))
        }
    else {
        output.table <- matrix("", nrow=2, ncol=5)
        rownames(output.table) <- c("group", "Residuals")
        colnames(output.table) <- c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
        tabel <- anova(lm(values.x ~ group.y))
        test <- oneway.test(values.x ~ group.y)
        output.table[ , 1] <- tabel$Df
        output.table[ , 2] <- round(tabel$Sum, 2)
        output.table[ , 3] <- round(tabel$Mean, 2)
        output.table[1, 4] <- round(test$statistic, 4)
        output.table[1, 5] <- round(test$p.value, 4)
        cat("ANOVA table using Welch approximation\n")
        print(noquote(output.table))
        return(invisible(list("Homogeneity of variances" = omog.test, "ANOVA" = list("anova"=tabel, "oneway.test"=test))))
        cat("\n")
        }
    }

