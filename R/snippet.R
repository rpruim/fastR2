snippet <-
function (name, execute = TRUE, view = !execute, echo = TRUE, 
    ask = getOption("demo.ask"), verbose = getOption("verbose"), 
    lib.loc = NULL, character.only = FALSE) 
{
    package <- "fastR"
    paths <- .find.package(package, lib.loc, verbose = verbose)
    paths <- paths[file_test("-d", file.path(paths, "snippet"))]
    if (missing(name)) {
        noName = TRUE
    }
    else {
        noName = FALSE
    }
    available <- character(0L)
    paths <- file.path(paths, "snippet")
    if (missing(name)) {
        for (p in paths) {
            files <- basename(tools::list_files_with_type(p, 
                "code"))
            available <- c(available, tools::file_path_sans_ext(files))
        }
        return(available)
    }
    for (p in paths) {
        files <- basename(tools::list_files_with_type(p, "code"))
        files <- files[name == tools::file_path_sans_ext(files)]
        if (length(files)) 
            available <- c(available, file.path(p, files))
    }
    if (!character.only) 
        name <- as.character(substitute(name))
    if (length(available) == 0L) 
        stop(gettextf("No snippet named '%s'", name), domain = NA)
    if (length(available) > 1L) {
        available <- available[1L]
        warning(gettextf("Snippet  '%s' found more than once,\nusing the one found in '%s'", 
            name, dirname(available[1L])), domain = NA)
    }
    if (ask == "default") 
        ask <- echo && grDevices::dev.interactive(orNone = TRUE)
    if (.Device != "null device") {
        oldask <- grDevices::devAskNewPage(ask = ask)
        on.exit(grDevices::devAskNewPage(oldask), add = TRUE)
    }
    op <- options(device.ask.default = ask)
    on.exit(options(op), add = TRUE)
    if (echo) {
        cat("\n\n", "\tsnippet(", name, ")\n", "\t------- ", 
            rep.int("~", nchar(name, type = "w")), "\n", sep = "")
    }
    if (view) {
        file <- srcfile(available)
        lines <- getSrcLines(file, 1, 5000)
        cat(paste(lines, collapse = "\n"))
    }
    if (execute) {
        if (ask && interactive()) {
            readline("\nType  <Return>\t to start : ")
        }
        source(available, echo = echo, max.deparse.length = Inf, 
            keep.source = TRUE)
    }
}
