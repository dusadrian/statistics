`t_testAB` <-
function(x, y = NULL,
         alternative = c("two.sided", "less", "greater"), var.equal = FALSE,
         mu = 0, paired = FALSE, conf.level = 0.95, data) {
    
    pmatching <- unique(c(match(alternative, c("!=", "<", ">")),
                        match(alternative, c("two.sided", "less", "greater"))))
    pmatching <- pmatching[!is.na(pmatching)]
    alternative <- c("two.sided", "less", "greater")[pmatching]
    
    if (!paired) {
        homogeneity <- c("are not equal", "are equal")
        
        if (is.null(y)) {
            homogtest <- ansari.test(x, data = data, exact = FALSE)
        }
        else {
            homogtest <- ansari.test(x, y, exact = FALSE)
        }
        
        p.value <- homogtest$p.value
        var.equal <- ifelse(p.value > (1 - conf.level), TRUE, FALSE)
        cat (
            "\nThe Ansari-Bradley test for the homogeneity of variances has a value of\n",
            paste("p =", round(p.value, 4), sep = ""),
            " therefore the variances are ",
            homogeneity[var.equal + 1],
            "\n",
            sep = ""
        )
    }
    
    if (is.null(y)) {
        test <- t.test(
            x,
            alternative = alternative,
            conf.level = conf.level,
            var.equal = var.equal,
            paired = paired, data = data
        )
        }
    else {
        test <- t.test(
            x,
            y,
            alternative = alternative,
            conf.level = conf.level,
            paired = paired,
            var.equal = var.equal
        )
        }
        
    print(test)
    
    if (paired) {
        return(invisible(list("t.test" = test)))
    }
    else {
        return(invisible(list("Homogeneity of variances" = homogtest, "t.test" = test)))
    }
}
