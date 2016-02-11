
snippify <- function(file, path, ...) {
  f <- file(file)
  lines <- readLines(f)
  close(f)

  results <- character(0)
  headers <- which(grepl("## ----", lines))
  if (length(headers) < 1) return(results)
  start <- headers
  end <- c(tail(headers,-1) - 1, length(lines))
  for (i in 1:length(start)) {
    message(lines[start[i]])
    chunkName <- sub("(## ----)(?<chunk>[^,]*)(.*)", "\\2", lines[start[i]], 
                     perl = TRUE)
    chunkName <- sub("-*$", "", chunkName)
    if (is.character(chunkName) && nchar(chunkName > 0) && !grepl("=", chunkName)) {
      chunkPath<- file.path(path, paste0(chunkName,".R"))
      message(paste("  ** Writing", chunkPath))
      results <- c(results, chunkPath)
      con <- file(chunkPath, "w")
      writeLines( lines[ (start[i] + 1) : end[i] ], con)
      close(con)
     
    }  
  }
}

require(knitr)

setwd("snipping")

purl("/Users/rpruim/projects/github/fast2e/Rnw/amsfast2.Rnw")
snippify("amsfast2.R", path="../../inst/snippet")

setwd("..")

