show_source <- 
  function(x, prompt=FALSE, highlight=TRUE, string.input=FALSE) {
  options = list(engine="R", prompt=prompt, highlight=highlight)
  if (! string.input) x <- deparse(substitute(x))
  paste0(
    "\\code{",
    knitr:::hilight_source(
      x, "latex", 
      options = options
    ),
    "}")
  }

