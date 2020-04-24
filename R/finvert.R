`finvert` <- function(variable, levels = FALSE) {
    # to do, same for haven_labelled
    if (!is.factor(variable)) {
        cat("\n")
        stop("The variable is not a factor.\n\n", call. = FALSE)
        }
    lista <- list(levels(variable), rev(levels(variable)))
    factor(variable, levels=lista[[1 + !levels]], labels=lista[[1 + levels]])
}

