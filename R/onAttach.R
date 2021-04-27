.onAttach <- function(...) {
    to_load <- c("admisc", "vctrs", "haven", "mixed")
    
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
    
    not_yet_loaded <- to_load[!is.element(paste0("package:", to_load), search())]

    if (length(not_yet_loaded) > 0) {

        packageStartupMessage(
            paste("Also attaching packages:", paste(not_yet_loaded, collapse = ", "))
        )

        suppressPackageStartupMessages(
            lapply(not_yet_loaded, load_library)
        )
    }

    return(invisible())

}
