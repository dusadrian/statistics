`t_testAB` <-
function(x, y = NULL,
         alternative = c("two.sided", "less", "greater"), var.equal = FALSE,
         mu = 0, paired = FALSE, conf.level = 0.95, data) {
    
    cautare <- unique(c(match(alternative, c("!=", "<", ">")),
                        match(alternative, c("two.sided", "less", "greater"))))
    cautare <- cautare[!is.na(cautare)]
    alternative <- c("two.sided", "less", "greater")[cautare]
    
    if (!paired) {
        omogenitate <- c("are not equal", "are equal")
        
        if (is.null(y)) {
            omog.test <- ansari.test(x, data=data, exact=FALSE)
            }
        else {
            omog.test <- ansari.test(x, y, exact=FALSE)
            }
        
        p.value <- omog.test$p.value
        var.equal <- ifelse(p.value > 0.05, TRUE, FALSE)
        cat ("\nThe Ansari-Bradley test for the homogeneity of variances has a value of\n",
             paste("p=", round(p.value, 4), sep=""), " therefore the variances are ",
             omogenitate[var.equal + 1], "\n", sep="")
        }
    
    if (is.null(y)) {
        test <- t.test(x, alternative=alternative, conf.level=conf.level, var.equal=var.equal, paired=paired, data=data)
        }
    else {
        test <- t.test(x, y, alternative=alternative, conf.level=conf.level, paired=paired, var.equal=var.equal)
        }
        
    print(test)
    
    if (paired) {
        return(invisible(list("t.test" = test)))
    }
    else {
        return(invisible(list("Homogeneity of variances" = omog.test, "t.test" = test)))
    }
}
