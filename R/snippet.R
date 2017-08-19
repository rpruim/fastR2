#' Display or execute a snippet of R code
#' 
#' This command will display and/or execute small snippets of R code from the
#' book \emph{Foundations and Applications of Statistics: An Introduction Using
#' R}.
#' 
#' \code{snippet} works much like \code{\link{demo}}, but the interface is
#' simplified.
#' 
#' @param name name of snippet
#' @param lib.loc character vector of directory names of R libraries, or NULL.
#' The default value of NULL corresponds to all libraries currently known.
#' @param character.only logical. If \code{TRUE}, use \code{name}as character
#' string.
#' @param verbose a logical. If \code{TRUE}, additional diagnostics are
#' printed.
#' @param echo a logical. If \code{TRUE}, show the R input when executing.
#' @param view a logical. If \code{TRUE}, snippet code is displayed 'as is'.
#' @param execute a logical.  If \code{TRUE}, snippet code is executed.  (The
#' code and the results of the execution will be visible if \code{echo} is
#' \code{TRUE}.)
#' @param ask a logical (or "default") indicating if
#' \code{devAskNewPage(ask=TRUE)} should be called before graphical output
#' happens from the snippet code. The value "default" (the factory-fresh
#' default) means to ask if \code{echo == TRUE} and the graphics device appears
#' to be interactive. This parameter applies both to any currently opened
#' device and to any devices opened by the demo code. If this is evaluated to
#' \code{TRUE} and the session is interactive, the user is asked to press
#' RETURN to start.
#' @param regex a logical indicating whether regular expression matching will 
#' be used.
#' @param max.files an integer limiting the number of files retrieved.
#' @author Randall Pruim
#' @seealso \code{\link{demo}}, \code{\link{source}}.
#' @keywords utilities
#' @importFrom utils file_test head
#' @export
snippet <-
function (name, execute = TRUE, view = !execute, echo = TRUE, 
    ask = getOption("demo.ask"), verbose = getOption("verbose"), 
    lib.loc = NULL, character.only = FALSE, regex = TRUE, max.files = 10L) 
{
    package <- "fastR2"
    paths <- find.package(package, lib.loc, verbose = verbose)
    paths <- paths[utils::file_test("-d", file.path(paths, "snippet"))]
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
    if (!character.only) 
        name <- as.character(substitute(name))
    for (p in paths) {
        snippet_files <- basename(tools::list_files_with_type(p, "code"))
        if (regex) {
          matching_files <- grep(name, snippet_files, value = TRUE)
        } else {
          matching_files <- files[name == tools::file_path_sans_ext(files)]
        }
        if (length(matching_files)) 
            available <- c(available, file.path(p, matching_files))
    }
    if (length(available) == 0L) 
        stop(gettextf("No snippet matching '%s'", name), domain = NA)
    if (length(available) > max.files) {
        available <- utils::head(available, max.files)
        warning(
          gettextf("Limiting to %i files.  Increase max.files if you want more.", max.files)
        )
    }
    if (ask == "default") 
        ask <- echo && grDevices::dev.interactive(orNone = TRUE)
    if (.Device != "null device") {
        oldask <- grDevices::devAskNewPage(ask = ask)
        on.exit(grDevices::devAskNewPage(oldask), add = TRUE)
    }
    op <- options(device.ask.default = ask)
    on.exit(options(op), add = TRUE)
    for (file in available) {
      fname <- basename(tools::file_path_sans_ext(file))
      if (echo) {
        cat(paste0("\n", "## snippet: ", fname, "\n"))
      }
      if (view) {
        lines <- getSrcLines(srcfile(file), 1, 5000)
        cat(paste(lines, collapse = "\n"))
      }
      if (execute) {
        if (ask && interactive()) {
          readline("\nType  <Return>\t to start : ")
        }
        source(file, echo = echo, max.deparse.length = Inf, 
               keep.source = TRUE)
      }
    }
}
