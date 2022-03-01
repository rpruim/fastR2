
snippify <- function(file, path, ...) {
  f <- file(file)
  lines <- readLines(f)
  close(f)

  chunkNames <- character(0)
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
    chunkNames <- append(chunkNames, chunkName)
    if (is.character(chunkName) && 
        nchar(chunkName > 0) && 
        ! grepl("-sol", chunkName) && 
        ! grepl("=", chunkName)) {
      chunkPath<- file.path(path, paste0(chunkName,".R"))
      message(paste("  ** Writing", chunkPath))
      results <- c(results, chunkPath)
      con <- file(chunkPath, "w")
      writeLines( lines[ (start[i] + 1) : end[i] ], con)
      close(con)
     
    }  
  }
  return(chunkNames)
}

require(knitr)

setwd("snipping")

purl("/Users/rpruim/projects/github/fast2e/Rnw/amsfast2.Rnw")
chunkNames <- snippify("amsfast2.R", path="../../inst/snippet")
RChunks <-
  dplyr::tibble(
    full_name = chunkNames,
    base_name = chunkNames %>% sub("[01].*", "", .) %>% sub("-fig|-sol", "", .),
    fig = grepl("-fig", chunkNames),
    sol = grepl("-sol", chunkNames)
  ) %>%
  filter(
    ! grepl(" ", full_name), 
    ! grepl("=", full_name), 
    nchar(full_name) > 0, 
    !sol, 
    !fig) %>%
  select(matches("name"))

devtools::use_data(RChunks, overwrite = TRUE, internal = TRUE)

setwd("..")

