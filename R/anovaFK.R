`anovaFK` <-
function(x, y = NULL, data) {
    
    m <- match.call()

    name_values_x <- NULL
    name_group_y <- NULL
    
    if (is.null(y)) {
        if (missing(x) || length(x) != 3) {
            stop("The formula is incorrect.\n\n", call. = FALSE)
        }
        name_values_x <- deparse(substitute(x)[[2]])
        name_group_y  <- deparse(substitute(x)[[3]])
        
        if (missing(data)) {
            values_x <- get(name_values_x, envir = parent.frame())
            group_y  <- get(name_group_y, envir = parent.frame())
            if (is.character(group_y)) {
                group_y <- as.factor(group_y)
            }
            homog_test <- fligner.test(values_x, group_y)
        }
        else {
            # homog_test <- fligner.test(x, data=data)
            values_x <- data[, name_values_x]
            group_y <- data[, name_group_y]
            if (is.character(group_y)) {
                group_y <- as.factor(group_y)
            }
            homog_test <- fligner.test(values_x, group_y)
        }
    }
    else {                     
        if (is.character(y)) {
            y <- as.factor(y)
        }
        homog_test <- fligner.test(x, y)
        values_x <- x
        group_y <- y
    }
    
    
    if (!is.factor(group_y)) {
        group_y <- as.factor(group_y)
    }
    
    
    
    if (homog_test$p.value > 0.05) {
        
        if (is.null(name_group_y)) {
            test <- aov(values_x ~ group_y)
        }
        else {
            testdf <- data.frame(values_x = values_x, group_y = group_y)
            colnames(testdf) <- c(name_values_x, name_group_y)
            test <- eval(parse(text = paste("aov(", name_values_x, " ~ ", name_group_y,")", sep = "")), envir = testdf)
        }
        
    }
    else {
        output_table <- matrix("", nrow=2, ncol=5)
        rownames(output_table) <- c("group", "Residuals")
        colnames(output_table) <- c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
        tabel <- anova(lm(values_x ~ group_y))
        if (is.null(name_group_y)) {
            test <- oneway.test(values_x ~ group_y)
        }
        else {
            testdf <- data.frame(values_x = values_x, group_y = group_y)
            colnames(testdf) <- c(name_values_x, name_group_y)
            test <- eval(parse(text = paste("oneway.test(", name_values_x, " ~ ", name_group_y,")", sep = "")), envir = testdf)
        }
        
        output_table[ , 1] <- tabel$Df
        output_table[ , 2] <- round(tabel$Sum, 2)
        output_table[ , 3] <- round(tabel$Mean, 2)
        output_table[1, 4] <- round(test$statistic, 4)
        output_table[1, 5] <- round(test$p.value, 4)
        

        attr(test, "output_table") <- output_table
    }

    attr(test, "homog_test") <- homog_test
    class(test) <- c("anovaFK", class(test))
    return(test)
}


`print.anovaFK` <- function(x, ...) {
    homog_test <- attr(x, "homog_test")
    homogeneity <- c("do not have equal variation (using Welch approximation).", "have equal variation.")
    
    var_equal <- ifelse(homog_test$p.value > 0.05, TRUE, FALSE)
    cat ("\nThe Fligner-Killeen test for the homogeneity of variances has a value of\n",
         paste("p = ", round(homog_test$p.value, 4), sep = ""), " so the groups ",
         homogeneity[var_equal + 1], "\n\n", sep = "")

    if (var_equal) {
        cat("ANOVA table\n\n")
        print(summary(x))
        cat("\n")
    }
    else {
        cat("ANOVA table using Welch approximation\n")
        print(noquote(attr(x, "output_table")))
        cat("\n")
    }
}
