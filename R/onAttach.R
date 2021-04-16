.onAttach <- function(...) {
    core <- c("admisc", "labelled", "haven")
    
    # code borrowed from package tidyverse
    
    # Attach the package from the same package library it was
    # loaded from before. https://github.com/tidyverse/tidyverse/issues/171
    load_library <- function(pkg) {
        if (pkg %in% loadedNamespaces()) {
            loc <- dirname(getNamespaceInfo(pkg, "path"))
            do.call(
                "library",
                list(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
            )
        }
    }
    
    not_yet_loaded <- core[!is.element(paste0("package:", core), search())]

    if (length(not_yet_loaded) > 0) {

        packageStartupMessage(
            paste("Also attaching packages:", paste(not_yet_loaded, collapse = ", "))
        )

        suppressPackageStartupMessages(
            lapply(not_yet_loaded, load_library)
        )
    }

    env <- asNamespace("haven")
    if (do.call("unlockEnvironment", list(env = env))) {
        do.call("unlockBinding", list(sym = "format_tagged_na", env = env))
        env$format_tagged_na <- function(x, digits = getOption("digits")) {
            out <- format(vctrs::vec_data(x), digits = digits)
            out[is_tagged_na(x)] <- paste0(".", na_tag(x)[is_tagged_na(x)])
            format(out, justify = "right")
        }
    }

    return(invisible())

}
